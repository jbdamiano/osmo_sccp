% SCCP user interface procedures

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

-module(sccp_user).
-behaviour(gen_server).

%-include_lib("osmo_ss7/osmo_util.hrl").
%-include_lib("osmo_ss7/sccp.hrl").
%-include_lib("osmo_ss7/mtp3.hrl").

% gen_fsm callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% our published API
-export([start_link/0]).

% client functions, may internally talk to our sccp_user server
-export([bind_ssn/2, unbind_ssn/2, pid_for_ssn/2, local_ssn_avail/2]).

-record(scu_state, {
	user_table
}).

-record(scu_record, {
	ssn_pc,
	user_pid
}).

% initialization code

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Arg) ->
	UserTbl = ets:new(sccp_user_tbl, [ordered_set, named_table,
				       {keypos, #scu_record.ssn_pc}]),
	{ok, #scu_state{user_table = UserTbl}}.

% client side code

bind_ssn(Ssn, Pc) ->
	gen_server:call(?MODULE, {bind_ssn, Ssn, Pc}).

unbind_ssn(Ssn, Pc) ->
	gen_server:call(?MODULE, {unbind_ssn, Ssn, Pc}).

% determine the pid registered for a given {Ssn, PC}
pid_for_ssn(Ssn, Pc) ->
	% as this is only a read access, we read the ets table directly
	% rather than going through call/2
	case ets:lookup(sccp_user_tbl, {Ssn, Pc}) of
	    [#scu_record{user_pid = UserPid}] ->
		{ok, UserPid};
	    _ ->
		{error, no_such_ssn}
	end.

local_ssn_avail(Ssn, Pc) ->
	case pid_for_ssn(Ssn, Pc) of
	    {ok, UserPid} ->
		true;
	    _ ->
		false
	end.

% server side code 

% bind a {SSN, PC} tuple to the pid of the caller
handle_call({bind_ssn, Ssn, Pc}, {FromPid, _FromRef}, S) ->
	#scu_state{user_table = Tbl} = S,
	NewRec = #scu_record{ssn_pc= {Ssn, Pc}, user_pid = FromPid},
	case ets:insert_new(Tbl, NewRec) of
	    false ->
		{reply, {error, ets_insert}, S};
	    Error ->
		% We need to trap the user Pid for EXIT
		% in order to automatically remove any SSN if
		% the user process dies
		link(FromPid),
		{reply, ok, S}
	end;

% unbind a {SSN, PC} tuple from the pid of the caller
handle_call({unbind_ssn, Ssn, Pc}, {FromPid, _FromRef}, S) ->
	#scu_state{user_table = Tbl} = S,
	DelRec = #scu_record{ssn_pc= {Ssn, Pc}, user_pid = FromPid},
	ets:delete_object(Tbl, DelRec),
	{reply, ok, S}.

handle_cast(Info, S) ->
	error_logger:error_report(["unknown handle_cast",
				  {module, ?MODULE},
				  {info, Info}, {state, S}]),
	{noreply, S}.

handle_info(Info, S) ->
	error_logger:error_report(["unknown handle_info",
				  {module, ?MODULE},
				  {info, Info}, {state, S}]),
	{noreply, S}.

terminate(Reason, _S) ->
	io:format("terminating ~p with reason ~p", [?MODULE, Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
