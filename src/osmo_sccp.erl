
-module(osmo_sccp).
-author('Harald Welte <laforge@gnumonks.org>').


-export([start/0, stop/0]).


start() ->
	application:start(tcap).

stop() ->
	application:stop(tcap).

