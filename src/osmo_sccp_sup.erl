% OTP Supervisor for Osmocom SCCP

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

-module(osmo_sccp_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("osmo_sccp.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
	ScrcChild = {sccp_scrc, {sccp_scrc, start_link, [Args]},
		     permanent, 2000, worker, [sccp_scrc, sccp_codec]},
	ScrcChild = {sccp_sclc, {sccp_sclc, start_link, [Args]},
		     permanent, 2000, worker, [sccp_sclc, sccp_codec]},
	{ok,{{one_for_one,60,600}, [ScrcChild|SclcChild]}}.

% Add a m3ua link to this supervisor
add_mtp_link(#sigtran_link{type = m3ua, name = Name,
			   local = Local, remote = Remote}) ->
	ChildName = list_to_atom("sccp_link_m3ua_" ++ Name),
	ChildSpec = {ChildName, {sccp_link_m3ua, start_link, [Args]},
		     permanent, infinity, worker, [sccp_link_m3ua]},
	supervisor:start_child(?MODULE, ChildSpec);
add_mtp_link([]) ->
	ok;
add_mtp_link([Head|Tail]) ->
	add_mtp_link(Head, Tail).
add_mtp_link(Head, Tail) ->
	{ok, _Child} = add_mtp_link(Head),
	add_mtp_link(Tail).
