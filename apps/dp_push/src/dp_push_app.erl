-module(dp_push_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([main/0, start/2, stop/1]).
-include("logger.hrl").
-include("types.hrl").

main() ->
    ssl:start(),
    application:start(dp_push),
    sync:go(),
    ok.


start(_StartType, _StartArgs) ->
    {ok, AProps} = application:get_env(apns),
    {ok, CProps} = application:get_env(cert),
    Apns = #apns{host = proplists:get_value(host, AProps),
		 port = proplists:get_value(port, AProps)},
    Cert = #cert{certfile = proplists:get_value(certfile, CProps),
		 password = proplists:get_value(password, CProps)},
    ?WARN("Server started ~p ~p ~n", [Apns, Cert]),
    dp_push_sup:start_link({Apns, Cert}).

    
stop(_State) ->
    ok.
