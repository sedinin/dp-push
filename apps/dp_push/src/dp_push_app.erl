-module(rolltogo_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([main/0, start/2, stop/1]).
-include("logger.hrl").

main() ->
    application:start(ranch),
    application:start(rolltogo),
    ok.

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),

    NumAcceptors = case application:get_env(in_production) of
		     {ok, false} -> ?WARN_("DEV MODE~n"),
				    sync:go(),
				    5;
		     _ -> ?WARN_("PRODUCTION MODE~n"),
			  {ok, NumAcc} = application:get_env(num_acceptors),
			  NumAcc
		 end,

    ?WARN("Server started at port ~p, num acceptors ~p~n", [Port, NumAcceptors]),

    {ok, DbOptions} = application:get_env(db_options),
    rolltogo_sup:start_link(DbOptions),

    ranch:start_listener(rolltogo, NumAcceptors,
			 ranch_tcp, [{port, Port}, {max_connections, 500000}],
			 rolltogo_handler, []).
    
    
stop(_State) ->
    ok.
