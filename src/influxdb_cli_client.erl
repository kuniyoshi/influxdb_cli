-module(influxdb_cli_client).
-export([now_to_mili_second/1]).
-export([get_options/0, set_options/1, update_options/1]).
-export([q/1]).
-export([update/1]).
-export([usage/0, usage/1]).
-define(APP, influxdb_cli).
-define(WORKER, influxdb_cli_worker).
-include_lib("eunit/include/eunit.hrl").

now_to_mili_second(Now) ->
    {MegaSecond, Second, MicroSecond} = Now,
    Milisecond = (MegaSecond * 1000 * 1000 + Second) * 1000 + MicroSecond / 1000,
    Milisecond2 = round(Milisecond),
    Milisecond2.

get_options() ->
    gen_server:call(?WORKER, get_options).

set_options(Options) ->
    gen_server:call(?WORKER, {set_options, Options}).

update_options(Options) ->
    gen_server:call(?WORKER, {update_options, Options}).

q(Q) ->
    gen_server:call(?WORKER, {q, Q}).

update(Points) ->
    gen_server:call(?WORKER, {update, Points}).

sample_points(Filename) ->
    {ok, [Points]} = file:consult(Filename),
    Points.

sample_points() ->
    Filename = filename:join(code:priv_dir(?APP), "sample_points.data"),
    sample_points(Filename).

usage(Options) ->
    Options2 = get_options(),
    ?debugVal(Options2),
    User = proplists:get_value(user, Options, "root"),
    Password = proplists:get_value(password, Options, "root"),
    Host = proplists:get_value(host, Options, "localhost"),
    Port = proplists:get_value(port, Options, 8086),
    Database = proplists:get_value(database, Options, "test"),
    Options3 = [{user, User},
                {password, Password},
                {host, Host},
                {port, Port},
                {database, Database}],
    ok = set_options(Options3),
    ok = update_options([{user, User}]),
    ?debugVal(q("list series")),
    q("delete from test"),
    update(jiffy:encode([{[{name, <<"test">>},
                           {columns, [<<"time">>, <<"tat">>]},
                           {points, sample_points()}]}])),
    ?debugVal(q("select tat from test group by time(1m)")).

usage() ->
    usage([]).
