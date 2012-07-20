-module(sender).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/1, send_alert/2]).
-export([test_msg/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("logger.hrl").
-include("types.hrl").

-record(state, {
	  apns :: #apns{},
	  cert :: #cert{}
	 }).

%%% module API

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).


-spec(send_alert(string(), device_token()) -> ok).
send_alert(Alert, DeviceToken) ->
    gen_server:cast(?MODULE, {send_alert, Alert, DeviceToken}),
    ok.

test_msg() ->
    gen_server:cast(?MODULE, test_msg).



%%% gen_server API

init({#apns{} = Apns, #cert{} = Cert}) ->
    ?INFO("~p inited with options ~p ~p ~n", [?MODULE, Apns, Cert]),
    {ok, #state{apns = Apns, cert = Cert}}.


handle_call(Any, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast({send_alert, Alert, DeviceToken}, #state{apns = Apns, cert = Cert} = State) ->
    Msg = apns:wrap_to_json(Alert),
    apns:send(apns:pack_simple(Msg, DeviceToken), Apns, Cert),
    {noreply, State};

handle_cast(test_msg, #state{apns = Apns, cert = Cert} = State) ->
    Msg = apns:test_msg(),
    DeviceToken = apns:test_device_token(),
    apns:send(Msg, DeviceToken, Apns, Cert),
    {noreply, State};

handle_cast(Any, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(Request, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	

