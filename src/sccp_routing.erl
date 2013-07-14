% SCCP routing code

% (C) 2011 by Harald Welte <laforge@gnumonks.org>
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
%
% If you modify this Program, or any covered work, by linking or
% combining it with runtime libraries of Erlang/OTP as released by
% Ericsson on http://www.erlang.org (or a modified version of these
% libraries), containing parts covered by the terms of the Erlang Public
% License (http://www.erlang.org/EPLICENSE), the licensors of this
% Program grant you additional permission to convey the resulting work
% without the need to license the runtime libraries of Erlang/OTP under
% the GNU Affero General Public License. Corresponding Source for a
% non-source form of such a combination shall include the source code
% for the parts of the runtime libraries of Erlang/OTP used as well as
% that of the covered work.

-module(sccp_routing).
-author('Harald Welte <laforge@gnumonks.org>').

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("osmo_ss7/include/mtp3.hrl").

-export([route_mtp3_sccp_in/1, route_local_out/1, select_opc/2]).

pointcode_is_local(Pc) ->
	PcInt = osmo_util:pointcode2int(Pc),
	ss7_links:is_pc_local(PcInt).

% local helper function
msg_return_or_cr_refusal(SccpMsg, RetCause, RefCause) ->
	case sccp_codec:is_connectionless(SccpMsg) of
	   true ->
		% if CL -> message return procedure
		message_return(SccpMsg, RetCause);
	   false ->
		% if CR -> connection refusal
		connection_refusal(SccpMsg, RefCause)
	end,
	{error, routing}.

% local outgoing CL or CR message
route_local_out(SccpMsg) when is_record(SccpMsg, sccp_msg) ->
	CalledParty = proplists:get_value(called_party_addr, SccpMsg#sccp_msg.parameters),
	#sccp_addr{global_title = Gt, ssn = Ssn, point_code = Pc} = CalledParty,
	if
	    (Gt == undefined) and ((Ssn == undefined) or (Ssn == 0)) ->
		% left-most colunm of Table 1/Q714 -> Action four
		Action = 4;
	    (Gt /= undefined) and ((Ssn == undefined) or (Ssn == 0)) ->
		% second (from left) column of Table 1/Q.714
		if (Pc == undefined) ->
			Action = 2;
		   true ->
			case pointcode_is_local(Pc) of
			    true ->
				Action = 2;
			    false ->
				Action = 3
			end
		end;
	    (Gt == undefined) and (Ssn /= undefined) ->
		% third (from left) column of Table 1/Q.714
		if (Pc == undefined) ->
			Action = 4;
		   true ->
			Action = 1
		end;
	    (Gt /= undefined) and (Ssn /= undefined) ->
		% last (from left) column of Table 1/Q.714
		if (Pc == undefined) ->
			Action = 2;
		   true ->
			if CalledParty#sccp_addr.route_on_ssn ->
				Action = 1;
			   true ->
				case pointcode_is_local(Pc) of
				    true ->
					Action = 2;
				    false ->
					Action = 3
				end
			end
		end
	end,
	route_local_out_action(Action, SccpMsg, CalledParty).

% select Originating Point Code for given (local_out) SCCP Msg
select_opc(SccpMsg, LsName) when is_record(SccpMsg, sccp_msg) ->
	% first try to find the Calling Party as specified by user
	case proplists:get_value(calling_party_addr,
				 SccpMsg#sccp_msg.parameters) of
	    undefined ->
		% no calling party: auto selection
		select_opc_auto(SccpMsg, LsName);
	    CallingParty ->
		case CallingParty#sccp_addr.point_code of
		    % calling party has no point code: auto selection
		    undefined ->
			select_opc_auto(SccpMsg, LsName);
		    Opc ->
			% calling party has point code: use it
			Opc
		end
	end.

select_opc_auto(SccpMsg, LsName) when is_record(SccpMsg, sccp_msg) ->
	% use SS7 link management to determine Opc
	ss7_links:get_opc_for_linkset(LsName).


% Acccording to 2.3.2 Action (1)
route_local_out_action(1, SccpMsg, CalledParty) ->
	#sccp_addr{global_title = Gt, ssn = Ssn, point_code = Pc} = CalledParty,
	case pointcode_is_local(Pc) of
	    true ->
		% c) procedures 2.3.1, item 2) are folloed
		case sccp_user:pid_for_ssn(Ssn, Pc) of
		    {ok, UserPid} ->
			% pass to either SCOC or SCLC
			{local, SccpMsg, UserPid};
		    {error, _Error} ->
			% message return / connection refusal
			msg_return_or_cr_refusal(SccpMsg,
						 ?SCCP_CAUSE_RET_UNEQUIP_USER,
						 ?SCCP_CAUSE_REF_UNEQUIPPED_USER)
		end;
	    false ->
		% If the DPC is not the node itself and the remote DPC, SCCP
		% and SSN are available, then the MTP-TRANSFER request
		% primitive is invoked unless the compatibility test returns
		% the message to SCLC or unless the message is discarded by the
		% traffic limitation mechanism;
		{ok, LsName} = ss7_routes:route_dpc(Pc),
		{remote, SccpMsg, LsName, Pc}
	end;

% Acccording to 2.3.2 Action (2)
route_local_out_action(2, SccpMsg, CalledParty) ->
	% perform GTT
	case gtt() of
	    undefined ->
		% if CL -> message return procedure
		% if CR -> connection refusal
		msg_return_or_cr_refusal(SccpMsg,
				?SCCP_CAUSE_RET_UNEQUIP_USER,
				?SCCP_CAUSE_REF_UNEQUIPPED_USER);
	    Dpc ->
		case pointcode_is_local(Dpc) of
		    true ->
			% message is passed, based on the message type, to
			% either SCOC or SCLC;
			{local, SccpMsg, undefined};
		    false ->
			% MTP-TRANSFER request primitive is invoked unless the
			% compatibility test returns the message to SCLC or
			% unless the message is discarded by the traffic
			% limitation mechanism
			{ok, LsName} = ss7_routes:route_dpc(Dpc),
			{remote, SccpMsg, LsName, Dpc}
		end
	end;

% Acccording to 2.3.2 Action (3)
route_local_out_action(3, SccpMsg, CalledParty) ->
	% The same actions as Action (1) apply, without checking the SSN.
	#sccp_addr{global_title = Gt, point_code = Pc} = CalledParty,
	case pointcode_is_local(Pc) of
	    true ->
		% pass to either SCOC or SCLC
		% theoretic case, as we only enter Action(3) for remote DPC
		{local, SccpMsg, undefined};
	    false ->
		% If the DPC is not the node itself and the remote DPC, SCCP
		% and SSN are available, then the MTP-TRANSFER request
		% primitive is invoked unless the compatibility test returns
		% the message to SCLC or unless the message is discarded by the
		% traffic limitation mechanism;
		{ok, LsName} = ss7_routes:route_dpc(Pc),
		{remote, SccpMsg, LsName, Pc}
	end;

% Acccording to 2.3.2 Action (4)
route_local_out_action(4, SccpMsg, CalledParty) ->
	% insufficient information.
	msg_return_or_cr_refusal(SccpMsg, ?SCCP_CAUSE_RET_NOTRANS_ADDR,
					  ?SCCP_CAUSE_REF_DEST_UNKNOWN).



route_cr_connless(Mtp3Msg, SccpMsg) when is_record(SccpMsg, sccp_msg) ->
	CalledParty = proplists:get_value(called_party_addr, SccpMsg#sccp_msg.parameters),
	case CalledParty#sccp_addr.route_on_ssn of
	    1 -> % sheet 3 (6)
		#sccp_addr{ssn = Ssn, point_code = Pc}= CalledParty,
		% check if the subsystem is available (FIXME: move this into SCLC ?!?)
		case sccp_user:pid_for_ssn(Ssn, Pc) of
		    {ok, UserPid} ->
			% forward to SCOC/SCLC
			{local, SccpMsg, UserPid};
		    {error, Error} ->
			% invoke connection refusal (if CR) or message return
			msg_return_or_cr_refusal(SccpMsg,
						 ?SCCP_CAUSE_RET_UNEQUIP_USER,
						 ?SCCP_CAUSE_REF_UNEQUIPPED_USER)
		end;
	    0 ->
		% Check for hop counter and increment it
		MsgPostHop = check_and_dec_hopctr(SccpMsg),
		MsgClass = proplists:get_value(?SCCP_PNC_PROTOCOL_CLASS,
						MsgPostHop#sccp_msg.parameters),
		%% FIXME: gtt() and others need to be implemented according to
		%% Q.714 C.1 sheet 2 and 3)
		#sccp_addr{ssn = Ssn, point_code = Pc}= CalledParty,
		% check if the subsystem is available (FIXME: move this into SCLC ?!?)
		case sccp_user:pid_for_ssn(Ssn, Pc) of
		    {ok, UserPid} ->
			% forward to SCOC/SCLC
			{local, SccpMsg, UserPid};
		    {error, Error} ->
			% invoke connection refusal (if CR) or message return
			msg_return_or_cr_refusal(SccpMsg,
						 ?SCCP_CAUSE_RET_UNEQUIP_USER,
						 ?SCCP_CAUSE_REF_UNEQUIPPED_USER)
		end

%		case MsgClass of
%		    0 ->
%			% FIXME: Assign SLS
%			ok;
%		    1 ->
%			% FIXME: Map incoming SLS to outgoing SLS
%			ok;
%		    _Default ->
%			ok
%		end,
%		% Optional screening function
%		% GTT needs to be performed
%		ok
	end.
	% FIXME: handle UDTS/XUDTS/LUDTS messages (RI=0 check) of C.1/Q.714 (1/12)
	% FIXME: handle translation already performed == yes) case of C.1/Q.714 (1/12)
	%route_main(SccpMsg),
	%LsName = ss7_routes:route_dpc(),
	%LsName = undefined,
	%{remote, SccpMsg, LsName, undefined}.


% CR or connectionless message, coming in from MTP
% return values
%	{local, SccpMsg, UserPid}
%	{remote}
route_mtp3_sccp_in(Mtp3Msg) when is_record(Mtp3Msg, mtp3_msg) ->
	{ok, Msg} = sccp_codec:parse_sccp_msg(Mtp3Msg#mtp3_msg.payload),
        %io:format("Parsed Msg: ~p~n", [Msg]),
	case Msg of
	    #sccp_msg{msg_type = ?SCCP_MSGT_CR} ->
		route_cr_connless(Mtp3Msg, Msg);
	    _ ->
		case sccp_codec:is_connectionless(Msg) of
		    true ->
			route_cr_connless(Mtp3Msg, Msg);
		    false ->
			{local, Msg, undefined}
		end
	end.

% Check if the message has a hop counter; decrement it if yes.
check_and_dec_hopctr(Msg = #sccp_msg{msg_type = MsgType}) when
			MsgType == ?SCCP_MSGT_XUDT;
			MsgType == ?SCCP_MSGT_XUDTS;
			MsgType == ?SCCP_MSGT_LUDT;
			MsgType == ?SCCP_MSGT_LUDTS;
			MsgType == ?SCCP_MSGT_CR ->
	HopCtr = proplists:get_value(?SCCP_PNC_HOP_COUNTER,
				     Msg#sccp_msg.parameters),
	if
	    HopCtr =< 1 ->
		% Error: Hop count expired
		io:format("SCCP hop count expired~n"),
		Msg;
	    true ->
		ParNew = lists:keyreplace(?SCCP_PNC_HOP_COUNTER, 1,
					  Msg#sccp_msg.parameters,
					  { ?SCCP_PNC_HOP_COUNTER, HopCtr -1}),
		Msg#sccp_msg{parameters = ParNew}
	end.

route_main(SccpMsg) when is_record(SccpMsg, sccp_msg) ->
	CalledParty = proplists:get_value(called_party_addr, SccpMsg#sccp_msg.parameters),
	case CalledParty#sccp_addr.point_code of
		undefined ->
			fixme
	end.


% Message return procedure (Section 4.2 / Q.714)
message_return(SccpMsg = #sccp_msg{msg_type = MsgType}, Cause) when
			MsgType == ?SCCP_MSGT_XUDT;
			MsgType == ?SCCP_MSGT_UDT;
			MsgType == ?SCCP_MSGT_LUDT ->
	% only return the message if the respective option is set
	{Class, Opt} = proplists:get_value(protocol_class, SccpMsg#sccp_msg.parameters),
	if Opt /= 8 ->
		ok;
	   true ->
		RetMsg = gen_ret_msg(SccpMsg, Cause),
		% FIXME: actually return it
		ok
	end;
message_return(_Msg, _Reason) ->
	ok.

% transform UDT/LUDT/XUDT into UDTS/LUDTS/XUDTS
gen_ret_msg(SccpMsg = #sccp_msg{msg_type = MsgType, parameters = Params}, Cause) ->
	% extract information fields required
	{Class, _Opt} = proplists:get_value(protocol_class, Params),
	RetMsgType = message_return_type(MsgType),
	CalledParty = proplists:get_value(called_party_addr, Params),
	CallingParty = proplists:get_value(calling_party_addr, Params),
	% build new options proplist
	Params1 = lists:keyreplace(called_party_addr, 1, Params,
				   {called_party_addr, CallingParty}),
	Params2 = lists:keyreplace(calling_party_addr, 1, Params1,
				   {calling_party_addr, CalledParty}),
	Params3 = [{return_cause, Cause}, {protocol_class, {Class, 0}}] ++ Params2,
	% return the new message
	SccpMsg#sccp_msg{msg_type = RetMsgType,
			 parameters = Params3}.

connection_refusal(SccpMsg = #sccp_msg{msg_type = ?SCCP_MSGT_CR}, Cause) ->
	CrefMsg = gen_cref_msg(SccpMsg, Cause),
	% FIXME: actually return it
	ok.

gen_cref_msg(SccpMsg = #sccp_msg{msg_type = ?SCCP_MSGT_CR, parameters =
		Params}, Cause) ->
	CalledParty = proplists:get_value(called_party_addr, Params),
	SrcLocalRef = proplists:get_value(src_local_ref, Params),
	CrefParams = [{dst_local_ref, SrcLocalRef},
		      {refusal_cause, Cause}],
	% FIXME: what about class / data/ ... ?
	#sccp_msg{msg_type = ?SCCP_MSGT_CREF, parameters = CrefParams}.

message_return_type(?SCCP_MSGT_XUDT) ->
	?SCCP_MSGT_XUDTS;
message_return_type(?SCCP_MSGT_UDT) ->
	?SCCP_MSGT_UDTS;
message_return_type(?SCCP_MSGT_LUDT) ->
	?SCCP_MSGT_LUDTS.

% dummy for now, we don't do GTT yet
gtt() ->
	undefined.
