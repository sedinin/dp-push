-module(dp_push_sup).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(supervisor).
-export([start_link/1, init/1]).


start_link(Options) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Options]).


init(Options) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
    Type = worker,       % worker | supervisor

    Sender = {dp_push_sender, % used to identify the child spec internally by the supervisor
	      {dp_push_sender, start_link, Options}, % StartFun = {M, F, A}
	      Restart, Shutdown, Type, 
	      [dp_push_sender]}, % Modules  = [Module] | dynamic

    {ok, {SupFlags, [Sender]}}.

