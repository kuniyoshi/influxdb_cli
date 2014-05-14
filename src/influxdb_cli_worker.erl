-module(influxdb_cli_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([call_get_options/1,
         call_set_options/2,
         call_update_options/2,
         call_q/2,
         call_update/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(State) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, State, []).

start_link() ->
    start_link([]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(State) ->
    {ok, State}.

handle_call({Action, Args}, _From, State) ->
    FunName = list_to_atom("call_" ++ atom_to_list(Action)),
    apply(fun ?MODULE:FunName/2, [Args, State]);
handle_call(Action, _from, State) ->
    FunName = list_to_atom("call_" ++ atom_to_list(Action)),
    apply(fun ?MODULE:FunName/1, [State]).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_port_string(Port) when is_integer(Port) ->
    integer_to_list(Port);
get_port_string(Port) ->
    Port.

gen_base_url(State) ->
    Host = proplists:get_value(host, State),
    PortString = get_port_string(proplists:get_value(port, State)),
    Database = proplists:get_value(database, State),
    User = proplists:get_value(user, State),
    Password = proplists:get_value(password, State),
    Url = lists:flatten(io_lib:format("http://~s:~s/db/~s/series?u=~s&p=~s",
                                      lists:map(fun http_uri:encode/1,
                                                [Host, PortString, Database, User, Password]))),
    Url.

gen_url(State, {q, Statement}) ->
    BaseUrl = gen_base_url(State),
    Url = BaseUrl ++ lists:flatten(io_lib:format("&q=~s", [http_uri:encode(Statement)])),
    Url.

query_http(Url) ->
    {ok, {_StatusLine, _Headers, Body}} = httpc:request(Url),
    jiffy:decode(Body).

call_get_options(State) ->
    {reply, State, State}.

call_set_options(NewState, _State) ->
    {reply, ok, NewState}.

call_update_options(NewState, State) ->
    State2 = lists:foldl(fun({Key, Value}, FoldState) ->
                    lists:keystore(Key, 1, FoldState, {Key, Value})
            end,
                         State,
                         NewState),
    {reply, ok, State2}.

call_q(Statement, State) ->
    Url = gen_url(State, {q, Statement}),
    Response = query_http(Url),
    {reply, Response, State}.

call_update(Json, State) ->
    Url = gen_base_url(State),
    {ok, {_StatusLine, _Headers, _Body}} = httpc:request(post,
                                                         {Url, [], "application/json", Json},
                                                         [],
                                                         []),
    {reply, ok, State}.
