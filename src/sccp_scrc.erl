% SCCP routing control procedures (SCRC)

% (C) 2010-2011 by Harald Welte <laforge@gnumonks.org>
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

-module(sccp_scrc).
-behaviour(gen_fsm).
-export([start_link/1, init/1, terminate/3, idle/2, handle_info/3]).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").



-record(scrc_state, {
		scoc_conn_ets,
		next_local_ref,
		user_pid	% pid() of the user process
	}).
% TODO: Integrate with proper SCCP routing / GTT implementation

tx_prim_to_local_ref(Prim, LocalRef) ->
	% determine the Pid to which the primitive must be sent
	ConnTable = get(scoc_by_ref),
	case ets:lookup(ConnTable, LocalRef) of
		[{LocalRef, ScocPid}] ->
			gen_fsm:send_event(ScocPid, Prim);
		_ ->
			io:format("Primitive ~p for unknown local reference ~p~n",
				  [Prim, LocalRef])
	end.


start_link(InitData) ->
	% make sure to store the Pid of the caller in the scrc_state
	gen_fsm:start_link({local, sccp_scrc}, sccp_scrc, 
			   [{user_pid,self()}|InitData], [{debug, [trace]}]).

% gen_fsm init callback, called by start_link()
init(InitPropList) ->
	io:format("SCRC Init PropList~p ~n", [InitPropList]),
	UserPid = proplists:get_value(user_pid, InitPropList),
	LoopData = #scrc_state{user_pid = UserPid, next_local_ref = 0},
	TableRef = ets:new(scoc_by_ref, [set]),
	put(scoc_by_ref, TableRef),
	ok = ss7_links:bind_service(?MTP3_SERV_SCCP, "osmo_sccp"),
	{ok, idle, LoopData}.


terminate(Reason, _State, _LoopDat) ->
	io:format("SCRC: Terminating with reason ~p~n", [Reason]),
	Tref = get(scoc_by_ref),
	ets:delete(Tref),
	ok.

% helper function to create new SCOC instance
spawn_new_scoc(LoopDat) ->
	% create new SCOC instance
	UserPid = LoopDat#scrc_state.user_pid,
	% Compute the new local reference
	LocalRef = LoopDat#scrc_state.next_local_ref + 1,
	LoopDat1 = LoopDat#scrc_state{next_local_ref = LocalRef},
	% generate proplist for SCRC initialization
	ScocPropList = [{scrc_pid, self()}, {user_pid, UserPid}, {local_reference, LocalRef}],
	{ok, ScocPid} = sccp_scoc:start_link(ScocPropList),
	% insert SCOC instance in connection table
	ConnTable = get(scoc_by_ref),
	ets:insert_new(ConnTable, {LocalRef, ScocPid}),
	{LoopDat1, ScocPid}.

is_cr_or_connless(SccpMsg) when is_record(SccpMsg, sccp_msg) ->
	case SccpMsg of
		#sccp_msg{msg_type = ?SCCP_MSGT_CR} ->
			true;
		_ ->
			sccp_codec:is_connectionless(SccpMsg)
	end.

% deliver message to local SCOC or SCLC
deliver_to_scoc_sclc(LoopDat, Msg, UserPid) when is_record(Msg, sccp_msg) ->
	case Msg of
		% special handling for CR message here in SCRC
		#sccp_msg{msg_type = ?SCCP_MSGT_CR} ->
			% spawn a new SCOC instance/process
			{LoopDat1, ScocPid} = spawn_new_scoc(LoopDat),
			% send a RCOC-CONNECTING.ind primitive to the new SCOC fsm
			UserPrim = osmo_util:make_prim('RCOC','CONNECTION', indication, Msg#sccp_msg.parameters),
			io:format("Sending ~p to ~p~n", [UserPrim, ScocPid]),
			gen_fsm:send_event(ScocPid, UserPrim),
			LoopDat1;
		% T(ias) expired on the other end of the connection
		%#sccp_msg{msg_type = ?SCCP_MSGT_IT} ->
		_ ->
			IsConnLess = sccp_codec:is_connectionless(Msg),
			case IsConnLess of
				true ->
					case UserPid of
					    undefined ->
						io:format("CL message to unequipped SSN~n");
					    _ ->
						% it would be more proper to send them via SCLC ??
						%gen_fsm:send(sccp_sclc, ??
						% FIXME: N-NOTICE.ind for NOTICE 
						UserPrim = osmo_util:make_prim('N','UNITDATA', indication, Msg),
						UserPid ! {sccp, UserPrim}
					end;
				false ->
					% connection oriented messages need to go via SCOC instance
					#sccp_msg{parameters = Opts} = Msg,
					LocalRef = proplists:get_value(dst_local_ref, Opts),
					ScocPrim = osmo_util:make_prim('RCOC', 'CONNECTION-MSG', indication, Msg),
					case LocalRef of
						undefined ->
							% FIXME: send SCCP_MSGT_ERR
							io:format("Conn-Msg to undefined ref ~p~n", [Msg]);
						_ ->
							tx_prim_to_local_ref(ScocPrim, LocalRef)
					end
			end,
			LoopDat
	end.


% N-CONNECT.req from user: spawn new SCOC and deliver primitive to it
idle(P = #primitive{subsystem = 'N', gen_name = 'CONNECT',
		    spec_name = request, parameters = Params}, LoopDat) ->
	% Start new SCOC instance
	{LoopDat1, ScocPid} = spawn_new_scoc(LoopDat),
	% Deliver primitive to new SCOC instance
	gen_fsm:send_event(ScocPid, P),
	{next_state, idle, LoopDat1};

% N-UNITDATA.req from user (normally this is SCLC, but we don't have SCLC)
idle(P= #primitive{subsystem = 'N', gen_name = 'UNITDATA',
		   spec_name = request, parameters = Params}, LoopDat) ->
	% User needs to specify: Protocol Class, Called Party, Calling Party, Data
	SccpMsg = #sccp_msg{msg_type = ?SCCP_MSGT_UDT, parameters = Params},
	case sccp_routing:route_local_out(SccpMsg) of
		{remote, SccpMsg2, LsName} ->
			% FIXME: get to MTP-TRANSFER.req
			{ok, M3} = create_mtp3_out(SccpMsg2, LsName),
			% generate a MTP-TRANSFER.req primitive to the lower layer
			send_mtp_transfer_down(LoopDat, M3, LsName),
			LoopDat1 = LoopDat;
		{local, SccpMsg2, UserPid} ->
			LoopDat1 = deliver_to_scoc_sclc(LoopDat, SccpMsg2, UserPid)
	end,
	{next_state, idle, LoopDat1};

% MTP-TRANSFER.ind from lower layer is passed into SCRC
idle(#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		spec_name = indication, parameters = Params}, LoopDat) ->
	case sccp_routing:route_mtp3_sccp_in(Params) of
		{remote, SccpMsg2, LsName} ->
			{ok, M3} = create_mtp3_out(SccpMsg2, LsName),
			% generate a MTP-TRANSFER.req primitive to the lower layer
			send_mtp_transfer_down(LoopDat, M3, LsName),
			LoopDat1 = LoopDat;
		{local, SccpMsg, UserPid} ->
			LoopDat1 = deliver_to_scoc_sclc(LoopDat, SccpMsg, UserPid)
	end,
	{next_state, idle, LoopDat1};
idle({sclc_scrc_connless_msg, SccpMsg}, LoopDat) ->
	% FIXME: see above, N-UNITDATA.req from user
	{next_state, idle, LoopDat};
% connection oriented messages like N-DATA.req from user
idle(#primitive{subsystem = 'OCRC', gen_name = 'CONNECTION-MSG',
		spec_name = request, parameters = Msg}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msg(Msg),
	% FIXME: routing and create_mtp3_out()
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat};
% SCOC has received confirmation about new incoming connection from user
idle(#primitive{subsystem = 'OCRC', gen_name = 'CONNECTION',
		spec_name = confirm, parameters = Params}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_CC, Params),
	% FIXME: routing and create_mtp3_out()
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat};


% triggered by N-CONNECT.req from user to SCOC:
idle(#primitive{subsystem = 'OCRC', gen_name = 'CONNECTION',
		spec_name = indication, parameters = Params}, LoopDat) ->
	% encode the actual SCCP message
	EncMsg = sccp_codec:encode_sccp_msgt(?SCCP_MSGT_CR, Params),
	% FIXME: routing and create_mtp3_out()
	% generate a MTP-TRANSFER.req primitive to the lower layer
	send_mtp_transfer_down(LoopDat, EncMsg),
	{next_state, idle, LoopDat}.

send_mtp_transfer_down(LoopDat, Mtp3) when is_record(Mtp3, mtp3_msg) ->
	ss7_links:mtp3_tx(Mtp3).

send_mtp_transfer_down(LoopDat, Mtp3, LsName) when is_record(Mtp3, mtp3_msg) ->
	ss7_links:mtp3_tx(Mtp3, LsName).

create_mtp3_out(SccpMsg, LsName) when is_record(SccpMsg, sccp_msg) ->
	CalledParty = proplists:get_value(called_party_addr,
					  SccpMsg#sccp_msg.parameters),
	% we _have_ to have a destination point code here
	Dpc = CalledParty#sccp_addr.point_code,
	case Dpc of
	    undefined ->
		{error, dpc_undefined};
	    _ ->
		Opc = sccp_routing:select_opc(SccpMsg, LsName),
		case Opc of
		    undefined ->
			{error, opc_undefined};
		    _ ->
			% FIXME: implement XUDT / LUDT support
			SccpEnc = sccp_codec:encode_sccp_msg(SccpMsg),
			% FIXME: select sls at random 
			M3R = #mtp3_routing_label{sig_link_sel = 0,
				  origin_pc = Opc,
				  dest_pc = Dpc},
			M3 = #mtp3_msg{network_ind = ?MTP3_NETIND_INTERNATIONAL,
				       service_ind = ?MTP3_SERV_SCCP,
				       routing_label = M3R,
				       payload = SccpEnc},
			{ok, M3}
		end
	end.

% FIXME: the MTP3 code should net send a gen_serve:cast ?!?
handle_info({'$gen_cast', P=#primitive{}}, State, LoopDat) ->
	#primitive{subsystem = 'MTP', gen_name = 'TRANSFER',
		spec_name = indication, parameters = Mtp3} = P,
	{ok, SccpMsg} = sccp_codec:parse_sccp_msg(Mtp3#mtp3_msg.payload),
	% User needs to specify: Protocol Class, Called Party, Calling Party, Data
	case sccp_routing:route_local_out(SccpMsg) of
		{error, routing} ->
			% routing tells us local subsystem not equipped
			LoopDat1 = LoopDat;
		{remote, SccpMsg2, LsName} ->
			% FIXME: get to MTP-TRANSFER.req
			{ok, M3} = create_mtp3_out(SccpMsg2, LsName),
			% generate a MTP-TRANSFER.req primitive to the lower layer
			send_mtp_transfer_down(LoopDat, M3, LsName),
			LoopDat1 = LoopDat;
		{local, SccpMsg2, UserPid} ->
			LoopDat1 = deliver_to_scoc_sclc(LoopDat, SccpMsg2, UserPid)
	end,
	{next_state, idle, LoopDat1}.

