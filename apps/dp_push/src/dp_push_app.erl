-module(dp_push_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([main/0, start/2, stop/1]).
-include("logger.hrl").


main() ->
    application:start(dp_push),
    sync:go(),
    ok.


start(_StartType, _StartArgs) ->
    {ok, Apns} = application:get_env(apns),
    {ok, Cert} = application:get_env(cert),
    ?WARN("Server started ~p ~p ~n", [Apns, Cert]),
    {ok, self()}.

    
stop(_State) ->
    ok.
