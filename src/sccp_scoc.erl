% ITU-T Q.71x SCCP Connection-oriented Control (SCOC)

% (C) 2010-2012 by Harald Welte <laforge@gnumonks.org>
%
% All Rights Reserved
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation; either version 3 of the
% License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(sccp_scoc).
-behaviour(gen_fsm).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").

-export([start_link/1]).

-export([init/1, handle_event/3]).
-export([idle/2, conn_pend_in/2, conn_pend_out/2, active/2, disconnect_pending/2,
	 reset_incoming/2, reset_outgoing/2, bothway_reset/2, wait_conn_conf/2]).

%% gen_fsm callbacks

% Appendix C.4 of Q.714 (all in milliseconds)
-define(CONNECTION_TIMER,	1  *60*100).
-define(TX_INACT_TIMER,		5  *60*100).
-define(RX_INACT_TIMER,		11 *60*100).
-define(RELEASE_TIMER,		10 *100).
-define(RELEASE_REP_TIMER,	10 *100).
-define(INT_TIMER,		1  *60*100).
-define(GUARD_TIMER,		23 *60*100).
-define(RESET_TIMER,		10 *100).
-define(REASSEMBLY_TIMER,	10 *60*100).

-record(state, {
	  role,			% client | server
	  user_application,	% {MonitorRef, pid()}
	  scrc_pid,		% pid()
	  rx_inact_timer,	% TRef
	  tx_inact_timer,	% TRef
	  local_reference,	% integer()
	  remote_reference,	% integer()
	  mtp3_label,		% mtp3_routing_label{}
	  class,		% {integer(), integer()}
	  user_pid		% pid()
	}).

% TODO: 
% 	expedited data
%	class 3
%	segmentation / reassembly

start_link(InitOpts) ->
	gen_fsm:start_link(sccp_scoc, InitOpts, [{debug, [trace]}]).

init(InitOpts) ->
	LoopDat = #state{user_pid=proplists:get_value(user_pid, InitOpts),
			 scrc_pid=proplists:get_value(scrc_pid, InitOpts),
			 local_reference=proplists:get_value(local_reference, InitOpts)},
	io:format("SCOC init Pid=~p LoopDat ~p~n", [self(), LoopDat]),
	{ok, idle, LoopDat}.

handle_event(stop, _StateName, LoopDat) ->
	io:format("SCOC received stop event~n"),
	{stop, normal, LoopDat};
handle_event({timer_expired, tx_inact_timer}, State, LoopDat) ->
	% FIXME: T(ias) is expired, send IT message
	io:format("T(ias) is expired, send IT message~n", []),
	Params = [{protocol_class, LoopDat#state.class},
		  {seq_segm, 0}, {credit, 0}],
	Prim = gen_co_sccp_prim(?SCCP_MSGT_IT, Params, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	{next_state, State, LoopDat};
handle_event({timer_expired, rx_inact_timer}, _State, LoopDat) ->
	io:format("T(iar) is expired, release connection~n", []),
	% Initiate connection release procedure
	disc_ind_stop_rel_3(LoopDat, ?SCCP_CAUSE_REL_SCCP_FAILURE).

% helper function to send a primitive to the user
send_user(_LoopDat = #state{user_pid = Pid}, Prim = #primitive{}) ->
	Pid ! {sccp, Prim}.

% low-level functions regarding activity timers
restart_tx_inact_timer(LoopDat) ->
	timer:cancel(LoopDat#state.tx_inact_timer),
	{ok, Tias} = timer:apply_after(?TX_INACT_TIMER, gen_fsm, send_all_state_event,
				 	[self(), {timer_expired, tx_inact_timer}]),
	LoopDat#state{tx_inact_timer = Tias}.

restart_rx_inact_timer(LoopDat) ->
	timer:cancel(LoopDat#state.rx_inact_timer),
	{ok, Tiar} = timer:apply_after(?RX_INACT_TIMER, gen_fsm, send_all_state_event,
				 	[self(), {timer_expired, rx_inact_timer}]),
	LoopDat#state{rx_inact_timer = Tiar}.

start_inact_timers(LoopDat) ->
	{ok, Tias} = timer:apply_after(?TX_INACT_TIMER, gen_fsm, send_all_state_event,
					[self(), {timer_expired, tx_inact_timer}]),
	{ok, Tiar} = timer:apply_after(?RX_INACT_TIMER, gen_fsm, send_all_state_event,
					[self(), {timer_expired, rx_inact_timer}]),
	LoopDat#state{rx_inact_timer = Tiar, tx_inact_timer = Tias}.

stop_inact_timers(#state{rx_inact_timer = Tiar, tx_inact_timer = Tias}) ->
	timer:cancel(Tiar),
	timer:cancel(Tias).


% -spec idle(#primitive{} | ) -> gen_fsm_state_return().

% STATE Idle

% N-CONNECT.req from user
idle(#primitive{subsystem = 'N', gen_name = 'CONNECT',
	        spec_name = request, parameters = Param}, LoopDat) ->
	% local reference already assigned in SCRC when instantiating this SCOC
	LocalRef = LoopDat#state.local_reference,
	% FIXME: determine protocol class and credit
	Class = {2,0},
	ParamDown = Param ++ [{src_local_ref, LocalRef}, {protocol_class, Class}],
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   osmo_util:make_prim('OCRC','CONNECTION', indication, ParamDown)),
	% start connection timer
	{next_state, conn_pend_out, LoopDat#state{class = Class}, ?CONNECTION_TIMER};

% RCOC-CONNECTION.req from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION',
		spec_name = indication, parameters = Params}, LoopDat) ->
	% associate remote reference to connection section
	RemRef = proplists:get_value(src_local_ref, Params),
	% determine the MTP3 label from Calling Party and/or MTP3 header
	Mtp3Label = determine_m3l_from_cr(Params),
	% determine protocol class and FIXME: credit
	Class = proplists:get_value(protocol_class, Params),
	LoopDat1 = LoopDat#state{remote_reference = RemRef, class = Class,
				 mtp3_label = mtp3_codec:invert_rout_lbl(Mtp3Label)},
	case LoopDat1#state.user_pid of
		undefined ->
			io:format("CR to unequipped subsystem!~n"),
			RefParam = [{refusal_cause, ?SCCP_CAUSE_REF_UNEQUIPPED_USER}],
			Prim = gen_co_sccp_prim(?SCCP_MSGT_CREF, RefParam, LoopDat1),
			gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
			{next_state, idle, LoopDat1};
		_ ->
			% send N-CONNECT.ind to user
			send_user(LoopDat1, osmo_util:make_prim('N', 'CONNECT', indication, [{scoc_pid, self()}|Params])),
			{next_state, conn_pend_in, LoopDat1}
	end;

% RCOC-ROUTING_FAILURE.ind from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'ROUTING FAILURE',
		spec_name = indication}, LoopDat) ->
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   osmo_util:make_prim('OCRC', 'CONNECTION REFUSED', indication)),
	{next_state, idle, LoopDat};

%FIXME: request type 2 ?!?

% RCOC-RELEASED.ind from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		spec_name = indication,
		parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RLSD}}, LoopDat) ->
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLC, [], LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	{next_state, idle, LoopDat};

% RCOC-RELEASE_COMPLETE.ind from SCRC
idle(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		spec_name = indication,
		parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RLC}}, LoopDat) ->
	{next_state, idle, LoopDat};

idle(#primitive{subsystem= 'RCOC', gen_name = 'DATA',
		spec_name = indication, parameters = Param}, LoopDat) ->
	% FIXME: if source reference, send error
	send_user(LoopDat, osmo_util:make_prim('N', 'DATA', indication, Param)),
	{next_state, idle, LoopDat}.

% STATE Connection pending incoming
conn_pend_in(#primitive{subsystem = 'N', gen_name = 'CONNECT',
			spec_name = response, parameters = Param}, LoopDat) ->
	io:format("SCOC N-CONNECT.resp LoopDat ~p~n", [LoopDat]),
	% assign local reference, SLS, protocol class and credit for inc section
	OutParam = [{dst_local_ref, LoopDat#state.remote_reference},
		    {src_local_ref, LoopDat#state.local_reference},
		    {protocol_class, LoopDat#state.class}] ++ Param,
	gen_fsm:send_event(LoopDat#state.scrc_pid,
			   osmo_util:make_prim('OCRC', 'CONNECTION', confirm, OutParam)),
	% start inactivity timers
	LoopDat1 = start_inact_timers(LoopDat),
	{next_state, active, LoopDat1};
conn_pend_in(any_npdu_type, LoopDat) ->
	{next_state, conn_pend_in, LoopDat};
conn_pend_in(#primitive{subsystem = 'N', gen_name = 'DISCONNECT',
			spec_name = request, parameters = Param}, LoopDat) ->
	% release resourcers (local ref may have to be released an frozen)
	Prim = gen_co_sccp_prim(?SCCP_MSGT_CREF, Param, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	{next_state, idle, LoopDat}.


disc_ind_stop_rel_3(LoopDat, RelCause) ->
	Params = [{release_cause, RelCause}],
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, osmo_util:make_prim('N', 'DISCONNECT',indication, Params)),
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLSD, Params, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	% start release timer
	{next_state, disconnect_pending, LoopDat, ?RELEASE_TIMER}.

rel_res_disc_ind_idle_2(LoopDat, Params) ->
	% release resources and local reference (freeze)
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, osmo_util:make_prim('N', 'DISCONNECT', indication, Params)),
	{next_state, idle, LoopDat}.


% STATE Connection pending outgoing
conn_pend_out(#primitive{subsystem = 'N', gen_name = 'DISCONNECT',
			 spec_name = request}, LoopDat) ->
	% FIXME: what about the connection timer ?
	{next_state, wait_conn_conf, LoopDat};
conn_pend_out(timeout, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat, [{refusal_cause, ?SCCP_CAUSE_REF_EXP_CONN_EST_TMR}]);
conn_pend_out(routing_failure, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat, [{refusal_cause, ?SCCP_CAUSE_REF_DEST_INACCESS}]);
conn_pend_out(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			 spec_name = indication,
			 parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RLSD,
						parameters = Params}}, LoopDat) ->
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLC, [], LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	rel_res_disc_ind_idle_2(LoopDat, Params);
% other N-PDU Type
conn_pend_out(other_npdu_type, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat, [{refusal_cause, ?SCCP_CAUSE_REF_INCOMP_USER_DATA}]);
conn_pend_out(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			 spec_name = indication,
			 parameters = #sccp_msg{msg_type = ?SCCP_MSGT_CREF,
						parameters = Params}}, LoopDat) ->
	rel_res_disc_ind_idle_2(LoopDat, Params);
conn_pend_out(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			 spec_name = indication,
			 parameters = #sccp_msg{msg_type = ?SCCP_MSGT_CC,
						parameters = Params}}, LoopDat) ->
	% start inactivity timers
	LoopDat1 = start_inact_timers(LoopDat),
	% assign protocol class and associate remote reference to connection
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	Mtp3Label = proplists:get_value(mtp3_label, Params),
	LoopDat2 = LoopDat1#state{remote_reference = SrcLocalRef,
				  mtp3_label = mtp3_codec:invert_rout_lbl(Mtp3Label)},
	% send N-CONNECT.conf to user
	send_user(LoopDat2, #primitive{subsystem = 'N', gen_name = 'CONNECT',
				       spec_name = confirm, parameters = Params}),
	{next_state, active, LoopDat2}.

stop_c_tmr_rel_idle_5(LoopDat) ->
	% stop connection timer (implicit)
	% release resources and local reference
	{next_state, idle, LoopDat}.

rel_freeze_idle(LoopDat) ->
	{next_state, idle, LoopDat}.

% STATE Wait connection confirmed
wait_conn_conf(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RLSD}}, LoopDat) ->
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLC, [], LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	stop_c_tmr_rel_idle_5(LoopDat);
wait_conn_conf(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_CC,
			  			 parameters = Params}}, LoopDat) ->
	% stop connection timer (implicit)
	% associate remote reference to connection section
	% assign protocol class and associate remote reference to connection
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	Mtp3Label = proplists:get_value(mtp3_label, Params),
	LoopDat2 = LoopDat#state{remote_reference = SrcLocalRef,
				 mtp3_label = mtp3_codec:invert_rout_lbl(Mtp3Label)},
	relsd_tmr_disc_pend_6(LoopDat2, ?SCCP_CAUSE_REL_USER_ORIG);
wait_conn_conf(other_npdu_type, LoopDat) ->
	% stop connection timer (implicit)
	rel_freeze_idle(LoopDat);
wait_conn_conf(timeout, LoopDat) ->
	stop_c_tmr_rel_idle_5(LoopDat);
wait_conn_conf(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_CREF}}, LoopDat) ->
	stop_c_tmr_rel_idle_5(LoopDat);
wait_conn_conf(routing_failure, LoopDat) ->
	stop_c_tmr_rel_idle_5(LoopDat).


relsd_tmr_disc_pend_6(LoopDat, RelCause) ->
	Params = [{release_cause, RelCause}],
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLSD, Params, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	% start release timer
	{next_state, disconnect_pending, LoopDat, ?RELEASE_TIMER}.

% STATE Active
active(#primitive{subsystem = 'N', gen_name = 'DISCONNECT',
		  spec_name = request}, LoopDat) ->
	% stop inactivity timers
	LoopDat1 = start_inact_timers(LoopDat),
	relsd_tmr_disc_pend_6(LoopDat1, ?SCCP_CAUSE_REL_USER_ORIG);
active(internal_disconnect, LoopDat) ->
	disc_ind_stop_rel_3(LoopDat, ?SCCP_CAUSE_REL_SCCP_FAILURE);
active(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		  parameters = #sccp_msg{msg_type = MsgType}}, LoopDat)
			when 	MsgType == ?SCCP_MSGT_CREF;
				MsgType == ?SCCP_MSGT_CC;
				MsgType == ?SCCP_MSGT_RLC ->
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	{next_state, active, LoopDat1};
active(#primitive{subsystem = 'RCOC', gen_name ='CONNECTION-MSG',
		  spec_name = indication,
		  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RLSD,
					 parameters = Params}}, LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'DISCONNECT',
				      spec_name = indication, parameters = Params}),
	% release resources and local reference (freeze)
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLC, [], LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	{next_state, idle, LoopDat};
active(error, LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'DISCONNECT',
				      spec_name = indication}),
	% release resources and local reference (freeze)
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLC, [], LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	{next_state, idle, LoopDat};
%active(rcv_inact_tmr_exp, LoopDat) ->
% this is handled in the global handle_event() above
active(routing_failure, LoopDat) ->
	% send N-DISCONNECT.ind to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'DISCONNECT',
				      spec_name = indication}),
	% stop inactivity timers
	stop_inact_timers(LoopDat),
	% start release timer
	{next_state, disconnect_pending, LoopDat, ?RELEASE_TIMER};
% Connection release procedures at destination node
%active(internal_disconnect) ->
% Data transfer procedures
active(#primitive{subsystem = 'N', gen_name = 'DATA',
		  spec_name = request, parameters = Param}, LoopDat) ->
	% FIXME Segment NSDU and assign value to bit M
	% FIXME handle protocol class 3
	gen_fsm:send_event(LoopDat#state.scrc_pid, {dt1, []}),
	% restart send inactivity timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	{next_state, active, LoopDat1};
active(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		  spec_name = indication,
		  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_DT1,
		  			 parameters = Params}}, LoopDat) ->
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	% FIXME handle protocol class 3
	% FIXME check for M-bit=1 and put data in Rx queue
	% N-DATA.ind to user
	UserData = proplists:get_value(user_data, Params),
	send_user(LoopDat1, osmo_util:make_prim('N', 'DATA', indication, {user_data, UserData})),
	{next_state, active, LoopDat1};
% Reset procedures
active(#primitive{subsystem = 'N', gen_name = 'RESET',
		  spec_name = request, parameters = _Param}, LoopDat) ->
	CausePar = [{reset_cause, ?SCCP_CAUSE_RES_ENDU_ORIGINATED}],
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RSR, CausePar, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	% start reset timer (implicit next_state below)
	% restart send inact timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	% reset variables and discard all queued and unacked msgs
	{next_state, reset_outgoing, LoopDat1, ?RESET_TIMER};
active(internal_reset_req, LoopDat) ->
	CausePar = [{reset_cause, ?SCCP_CAUSE_RES_SCCP_USER_ORIG}],
	% N-RESET.ind to user
	send_user(LoopDat, osmo_util:make_prim('N', 'RESET', indication,
						CausePar)),
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RSR, CausePar, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	% start reset timer
	% restart send inact timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	% reset variables and discard all queued and unacked msgs
	{next_state, bothway_reset, LoopDat1, ?RESET_TIMER};
active(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		  spec_name = indication,
		  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_IT,
					 parameters = Params}}, LoopDat) ->
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	% Section 3.4 Inactivity control
	SrcRef = proplists:get_value(src_local_ref, Params),
	case LoopDat1#state.remote_reference of
		SrcRef ->
			ClassOpt = proplists:get_value(protocol_class, Params),
			case LoopDat1#state.class of
				ClassOpt ->
					% FIXME: class3: discrepancy in seq/segm or credit -> reset
					{next_state, active, LoopDat1};
				_ ->
					% discrepancy in class -> release
					disc_ind_stop_rel_3(LoopDat1, ?SCCP_CAUSE_REL_INCONS_CONN_DAT)
			end;
		_ ->
			% discrepancy in src ref -> release
			disc_ind_stop_rel_3(LoopDat1, ?SCCP_CAUSE_REL_INCONS_CONN_DAT)
	end;
active(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		  spec_name = indication,
		  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RSC}}, LoopDat) ->
	% discard received message
	{next_state, active, LoopDat};
active(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
		  spec_name = indication,
		  parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RSR,
		  			 parameters = Params}}, LoopDat) ->
	% restart send inactivity timer
	LoopDat1 = restart_tx_inact_timer(LoopDat),
	% N-RESET.ind to user
	send_user(LoopDat1, osmo_util:make_prim('N', 'RESET', indication, Params)),
	% reset variables and discard all queued and unacked msgs
	{next_state, reset_incoming, LoopDat1}.

rel_res_stop_tmr_12(LoopDat) ->
	% release resources and local reference (freeze)
	% stop release and interval timers
	{next_state, idle, LoopDat}.

% STATE Disconnect pending
disconnect_pending(#primitive{subsystem = 'RCOC', gen_name = 'CONNECTION-MSG',
			      spec_name = indication,
			      parameters = #sccp_msg{msg_type = ?SCCP_MSGT_RLC}}, LoopDat) ->
	rel_res_stop_tmr_12(LoopDat);
disconnect_pending(released_error, LoopDat) ->
	rel_res_stop_tmr_12(LoopDat);
disconnect_pending(routing_failure, LoopDat) ->
	{next_state, disconnect_pending, LoopDat};
disconnect_pending(other_npdu_type, LoopDat) ->
	% discared received message
	{next_state, disconnect_pending, LoopDat};
disconnect_pending(timeout, LoopDat) ->
	% FIXME: store the original release cause and use same cause here
	Params = [{release_cause, ?SCCP_CAUSE_REL_UNQUALIFIED}],
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLSD, Params, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	% FIXME: start interval timer
	% start repeat release timer
	{next_state, disconnect_pending, ?RELEASE_REP_TIMER};
disconnect_pending(intv_tmr_exp, LoopDat) ->
	% inform maintenance
	rel_res_stop_tmr_12(LoopDat);
% FIXME: this is currently ending up in normal 'timeout' above
disconnect_pending(repeat_release_tmr_exp, LoopDat) ->
	Params = [{release_cause, ?SCCP_CAUSE_REL_UNQUALIFIED}],
	Prim = gen_co_sccp_prim(?SCCP_MSGT_RLSD, Params, LoopDat),
	gen_fsm:send_event(LoopDat#state.scrc_pid, Prim),
	% FIXME restart repeat release timer
	{next_state, disconnect_pending}.

res_out_res_conf_req(LoopDat) ->
	% N-RESET.conf to user
	send_user(LoopDat, osmo_util:make_prim('N', 'RESET', confirm)),
	% stop reset timer (implicit)
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	% resume data transfer
	{next_state, active, LoopDat1}.

% STATE Reset outgoing
reset_outgoing(#primitive{subsystem = 'N', gen_name = 'DATA',
			  spec_name = request, parameters = Params}, LoopDat) ->
	% FIXME received information ?!?
	{next_state, reset_outgoing, LoopDat};
reset_outgoing(#primitive{subsystem = 'N', gen_name = 'EXPEDITED DATA',
			  spec_name = request, parameters = Params}, LoopDat) ->
	% FIXME received information ?!?
	{next_state, reset_outgoing, LoopDat};
reset_outgoing(timeout, LoopDat) ->
	% FIXME check for temporary connection section
	% inform maintenance
	{next_state, maintenance_Blocking, LoopDat};
%reset_outgoing(error, LoopDat) ->
%reset_outgoing(released, LoopDat) ->
reset_outgoing(other_npdu_type, LoopDat) ->
	% discard received message
	{next_state, reset_outgoing, LoopDat};
reset_outgoing(reset_confirm, LoopDat) ->
	res_out_res_conf_req(LoopDat);
reset_outgoing(reset_request, LoopDat) ->
	res_out_res_conf_req(LoopDat).

bway_res_req_resp(LoopDat) ->
	{next_state, reset_outgoing, LoopDat}.

bway_res_res_conf_req(LoopDat) ->
	% N-RESET.conf to user
	send_user(LoopDat, #primitive{subsystem = 'N', gen_name = 'RESET',
				      spec_name = confirm}),
	% stop reset timer (implicit)
	% restart receive inactivity timer
	LoopDat1 = restart_rx_inact_timer(LoopDat),
	{next_state, reset_incoming, LoopDat1}.

% STATE Bothway Reset
bothway_reset(#primitive{subsystem = 'N', gen_name = 'RESET',
			 spec_name = request, parameters = Params}, LoopDat) ->
	bway_res_req_resp(LoopDat);
bothway_reset(#primitive{subsystem = 'N', gen_name = 'RESET',
			 spec_name = response, parameters = Params}, LoopDat) ->
	bway_res_req_resp(LoopDat);
bothway_reset(timeout, LoopDat) ->
	% FIXME check for temporary connection section
	% inform maintenance
	{next_state, maintenance_Blocking, LoopDat};
%bothway_reset(error, LoopDat) ->
%bothway_reset(released, LoopDat) ->
bothway_reset(other_npdu_type, LoopDat) ->
	% discard received message
	{next_state, bothway_reset, LoopDat}.

% STATE Reset incoming
reset_incoming(#primitive{subsystem = 'N', gen_name = 'RESET',
			  spec_name = request, parameters = Params}, LoopDat) ->
	% received information
	{nest_state, reset_incoming, LoopDat};
%reset_incoming(error, LoopDat) ->
%reset_incoming(released, LoopDat) ->
reset_incoming(other_npdu_type, LoopDat) ->
	% discard received message
	% internal reset request
	{next_state, active, LoopDat}.
% FIXME: response or request
%reset_incoming(


msg_has(MsgType, src_local_ref, LoopDat) when
		MsgType == ?SCCP_MSGT_CR;
		MsgType == ?SCCP_MSGT_CC;
		MsgType == ?SCCP_MSGT_RLSD;
		MsgType == ?SCCP_MSGT_RLC;
		MsgType == ?SCCP_MSGT_RSR;
		MsgType == ?SCCP_MSGT_RSC;
		MsgType == ?SCCP_MSGT_IT ->
	[{src_local_ref, LoopDat#state.local_reference}];
msg_has(MsgType, dst_local_ref, LoopDat) when
		MsgType == ?SCCP_MSGT_CR;
		MsgType == ?SCCP_MSGT_CC;
		MsgType == ?SCCP_MSGT_CREF;
		MsgType == ?SCCP_MSGT_RLSD;
		MsgType == ?SCCP_MSGT_RLC;
		MsgType == ?SCCP_MSGT_DT1;
		MsgType == ?SCCP_MSGT_DT2;
		MsgType == ?SCCP_MSGT_AK;
		MsgType == ?SCCP_MSGT_ED;
		MsgType == ?SCCP_MSGT_RSR;
		MsgType == ?SCCP_MSGT_RSC;
		MsgType == ?SCCP_MSGT_ERR;
		MsgType == ?SCCP_MSGT_IT ->
	[{dst_local_ref, LoopDat#state.remote_reference}];
msg_has(MsgType, _, _LoopDat) ->
	[].

% generate a Connection Oriented SCCP message, automatically adding src and dst
% local reference if required for the specific message type
gen_co_sccp(MsgType, ParamsIn, LoopDat) when is_record(LoopDat, state) ->
	Params = msg_has(MsgType, src_local_ref, LoopDat) ++
		 msg_has(MsgType, dst_local_ref, LoopDat),
	#sccp_msg{msg_type = MsgType, parameters = ParamsIn ++ Params}.

% generate a OCRC primitive containing a connection oriented SCCP message
gen_co_sccp_prim(MsgType, ParamsIn, LoopDat) when is_record(LoopDat, state) ->
	Label = LoopDat#state.mtp3_label,
	Sccp = gen_co_sccp(MsgType, ParamsIn, LoopDat),
	osmo_util:make_prim('OCRC', 'CONNECTION-MSG', request, [Sccp, Label]).

% According to Q.714 2.7 d)
determine_m3l_from_cr(Params) ->
	M3l = proplists:get_value(mtp3_label, Params),
	% if there is no calling party, or no point code in the calling party,
	% we have to use the MTP3 OPC as point code for the 'connection section'
	case proplists:get_value(calling_party_addr, Params) of
		undefined ->
			M3l;
		#sccp_addr{point_code = undefined} ->
			M3l;
		#sccp_addr{point_code = Spc} ->
			M3l#mtp3_routing_label{origin_pc = Spc}
	end.
