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
%
% Additional Permission under GNU AGPL version 3 section 7:
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

-module(osmo_sccp_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("osmo_sccp.hrl").

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(Args) ->
	ScrcChild = {sccp_scrc, {sccp_scrc, start_link, [Args]},
		     permanent, 2000, worker, [sccp_scrc, sccp_codec, sccp_routing]},
	UserChild = {sccp_user, {sccp_user, start_link, []},
		     permanent, 2000, worker, [sccp_user]},
	{ok,{{one_for_one,60,600}, [ScrcChild, UserChild]}}.
