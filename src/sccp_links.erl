% Internal SCCP link database keeping

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

-module(sccp_links).
-behaviour(gen_server).

% gen_fsm callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

% our published API
-export([start_link/0]).

% client functions, may internally talk to our sccp_user server
-export([register_linkset/3, unregister_linkset/1]).
-export([register_link/3, unregister_link/2, set_link_state/3]).
-export([get_pid_for_link/2]).

-record(slink, {
	key,		% {linkset_name, sls}
	name,
	linkset_name,
	sls,
	user_pid,
	state
}).

-record(slinkset, {
	name,
	local_pc,
	remote_pc,
	user_pid,
	state,
	links
}).

-record(su_state, {
	linkset_tbl,
	link_tbl
}).


% initialization code

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Arg) ->
	LinksetTbl = ets:new(sccp_linksets, [ordered_set,
					     {keypos, #slinkset.name}]),

	% create a named table so we can query without reference directly
	% within client/caller process
	LinkTbl = ets:new(sccp_link_table, [ordered_set, named_table,
					    {keypos, #slink.key}]),
	{ok, #su_state{linkset_tbl = LinksetTbl, link_tbl = LinkTbl}}.

% client side API

% all write operations go through gen_server:call(), as only the ?MODULE
% process has permission to modify the table content

register_linkset(LocalPc, RemotePc, Name) ->
	gen_server:call(?MODULE, {register_linkset, {LocalPc, RemotePc, Name}}).

unregister_linkset(Name) ->
	gen_server:call(?MODULE, {unregister_linkset, {Name}}).

register_link(LinksetName, Sls, Name) ->
	gen_server:call(?MODULE, {register_link, {LinksetName, Sls, Name}}).

unregister_link(LinksetName, Sls) ->
	gen_server:call(?MODULE, {unregister_link, {LinksetName, Sls}}).

set_link_state(LinksetName, Sls, State) ->
	gen_server:call(?MODULE, {set_link_state, {LinksetName, Sls, State}}).

% the lookup functions can directly use the ets named_table from within
% the client process, no need to go through a synchronous IPC

get_pid_for_link(LinksetName, Sls) ->
	case ets:lookup(sccp_link_table, {LinksetName, Sls}) of
	    [#slink{user_pid = Pid}] ->	
		% FIXME: check the link state 
		{ok, Pid};
	    _ ->
		{error, no_such_link}
	end.

% server side code

handle_call({register_linkset, {LocalPc, RemotePc, Name}},
				{FromPid, _FromRef}, S) ->
	#su_state{linkset_tbl = Tbl} = S,
	Ls = #slinkset{local_pc = LocalPc, remote_pc = RemotePc,
		       name = Name, user_pid = FromPid},
	case ets:insert_new(Tbl, Ls) of
	    false ->
		{reply, {error, ets_insert}, S};
	    _ ->
		% We need to trap the user Pid for EXIT
		% in order to automatically remove any links/linksets if
		% the user process dies
		link(FromPid),
		{reply, ok, S}
	end;

handle_call({unregister_linkset, {Name}}, {FromPid, _FromRef}, S) ->
	#su_state{linkset_tbl = Tbl} = S,
	ets:delete(Tbl, Name),
	{reply, ok, S};

handle_call({register_link, {LsName, Sls, Name}},
				{FromPid, _FromRef}, S) ->
	#su_state{linkset_tbl = LinksetTbl, link_tbl = LinkTbl} = S,
	% check if linkset actually exists
	case ets:lookup(LinksetTbl, LsName) of
	    [#slinkset{}] ->
		Link = #slink{name = Name, sls = Sls,
			      user_pid = FromPid, key = {LsName, Sls}},
		case ets:insert_new(LinkTbl, Link) of
		    false ->
			{reply, {error, link_exists}, S};
		    _ ->
			% We need to trap the user Pid for EXIT
			% in order to automatically remove any links if
			% the user process dies
			link(FromPid),
			{reply, ok, S}
		end;
	    _ ->
		{reply, {error, no_such_linkset}, S}
	end;

handle_call({unregister_link, {LsName, Sls}}, {FromPid, _FromRef}, S) ->
	#su_state{link_tbl = LinkTbl} = S,
	ets:delete(LinkTbl, {LsName, Sls}),
	{reply, ok, S};

handle_call({set_link_state, {LsName, Sls, State}}, {FromPid, _}, S) ->
	#su_state{link_tbl = LinkTbl} = S,
	case ets:lookup(LinkTbl, {LsName, Sls}) of
	    [] ->
		{reply, {error, no_such_link}, S};
	    [Link] ->
		NewLink = Link#slink{state = State},
		ets:insert(LinkTbl, NewLink),
		{reply, ok, S}
	end.

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
