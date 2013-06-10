% wrapper code between signerl/TCAP and osmo_ss7/SCCP

-module(osmo_sccp_tcap).

-copyright('Copyright (C) 2011 by Harald Welte <laforge@gnumonks.org>').
-author('Harald Welte <laforge@gnumonks.org>').

-behaviour(tcap_tco_server).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").
-include_lib("TCAP/include/sccp.hrl").

%% callbacks needed for gen_server behaviour
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% callbacks needed for tcap_tco_server
-export([send_primitive/2, start_user/3]).

%% our published API functions
-export([start_link/1, stop/1]).

start_link(SSN) ->
	Ourname = list_to_atom("sccp_ssn" ++ integer_to_list(SSN)),
	gen_server:start_link({local, Ourname}, ?MODULE, [SSN],[{debug,trace}]).

init([SSN]) when is_integer(SSN) ->
	init(SSN);
init(SSN) when is_integer(SSN) ->
	ok = sccp_user:bind_ssn(SSN, undefined),
	State = 1,
	{ok, State}.

handle_call(stop, _From, State) ->
	{stop, shutdown, State}.

handle_cast(Request, State) ->
	error_logger:error_report(["unknown handle_cast",
				   {module, ?MODULE},
				   {request, Request}, {state, State}]),
	{noreply, State}.

osmo_prim2signerl(#primitive{subsystem='N', gen_name='UNITDATA',
			     spec_name=Spec, parameters=Msg}) ->
	Params = Msg#sccp_msg.parameters,
	CalledAddr = proplists:get_value(called_party_addr, Params),
	CallingAddr = proplists:get_value(calling_party_addr, Params),
	UserData = proplists:get_value(user_data, Params),
	% FIXME: doesn't always exist!
	%{PC, Opt} = proplists:get_value(protocol_class, Params),
	Rec = #'N-UNITDATA'{calledAddress = CalledAddr,
			    callingAddress = CallingAddr,
			    sequenceControl = undefined,
			    returnOption = undefined,
			    importance = undefined,
		    	    userData = UserData},
	{'N','UNITDATA', Spec, Rec}.

% incoming message from
handle_info({sccp, P= #primitive{subsystem='N',
				 gen_name='UNITDATA',
				 spec_name=indication}}, State) ->
	% this is really ugly, we need to make TCO understand #primitives
	gen_server:cast(self(), osmo_prim2signerl(P)),
	{noreply, State};
handle_info({sccp, P= #primitive{subsystem='N',
				 gen_name='NOTICE',
				 spec_name=indication}}, State) ->
	% this is really ugly, we need to make TCO understand #primitives
	%gen_server:cast(self(), osmo_prim2signerl(P)),
	error_logger:error_report(["unimplemented N-NOTICE.ind",
				   {module, ?MODULE},
				   {sccp, P}, {state, State}]),
	{noreply, State};
handle_info(Info, State) ->
	error_logger:error_report(["unknown handle_info",
				   {module, ?MODULE},
				   {info, Info}, {state, State}]),
	{noreply, State}.

terminate(Reason, State) ->
	io:format("osmo_sccp_tcap terminating with Reason ~w", [Reason]),
	ok.

%% @spec (NSAP) -> ok
%% 	NSAP = pid()
%%
%% @doc Stop an sccp server.
%% 	<p>Closes an SCCP service access point (SAP).</p>
%% 	<p><tt>NSAP</tt> is a pid returned from a previous call to
%% 	<tt>start_link/2,3,7</tt>.</p>
%%
stop(NSAP) ->
	gen_server:call(NSAP, stop).

% UNITDATA.req

% message coming down from TCO to SCCP, to be transmitted
send_primitive({'N', 'UNITDATA', request, #'N-UNITDATA'{calledAddress = Called,
						     callingAddress = Calling,
						     sequenceControl = Seq,
						     returnOption = RetOpt,
						     importance = Imp,
						     userData = UserData}=Par}, State) ->
	io:format("N-UNITDATA.req (~w,~w)~n", [Par, State]),
	ClassOut = protocol_class(tcap_to_osmo, {Seq, RetOpt}),
	UserDataOut = iolist_to_binary(UserData),
	% Build an osmo_ss7 primitive
	Prim = osmo_util:make_prim('N', 'UNITDATA', request,
				       [{called_party_addr, Called},
					{calling_party_addr, Calling},
					{protocol_class, ClassOut},
					{user_data, UserDataOut}]),
	% send primitive to SCCP code
	gen_fsm:send_event(sccp_scrc, Prim).


% TCAP has received the start of a new dialogue, return pid() of new user
start_user({DhaPid, CcoPid}, DialogueID, State) ->
	tcap_test_user:start_link({DhaPid,CcoPid}, DialogueID).


% convert signerl format to osmo format
protocol_class(tcap_to_osmo, {SeqIn, ReturnIn}) ->
	case SeqIn of
	    true ->
		Class = 1;
	    _ ->
		Class = 0
	end,
	case ReturnIn of
	    true ->
		{Class, 8};
	    _ ->
		{Class, 0}
	end.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
