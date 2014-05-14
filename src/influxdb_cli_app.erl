-module(influxdb_cli_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(WORKER, influxdb_cli_worker).

start(_StartType, _StartArgs) ->
    influxdb_cli_sup:start_link().

stop(_State) ->
    ok.
