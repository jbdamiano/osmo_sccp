% SCCP subsystem for dumping SCCP messages (testing)

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

-module(sccp_ssn_dump).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_info/2]).

-include_lib("osmo_ss7/include/osmo_util.hrl").
-include_lib("osmo_ss7/include/sccp.hrl").

-record(loop_dat, {
	ssns
}).

start_link(Ssns) ->
	gen_server:start_link(sccp_ssn_dump, [Ssns], []).

init([Ssn]) when is_tuple(Ssn) ->
	init([[Ssn]]);
init([Ssns]) when is_list(Ssns) ->
	bind_ssns(Ssns),
	{ok, #loop_dat{ssns = Ssns}}.

bind_ssns([]) ->
	ok;
bind_ssns([{Ssn, Pc}|Tail]) ->
	io:format("binding ~p ~p~n", [Ssn, Pc]),
	ok = sccp_user:bind_ssn(Ssn, Pc),
	bind_ssns(Tail).


handle_info({sccp, Prim}, LoopDat) ->
	io:format("sccp_ssn_dump in: ~p~n", [Prim]),
	{noreply, LoopDat}.
