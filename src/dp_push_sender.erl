-module(dp_push_sender).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/1, send/2, send_alert/2, send_badge/2, send_data/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([test_send/0, test_feedback/0]).
-include("logger.hrl").
-include("types.hrl").

-record(state, {
	  apns :: #apns{},
	  cert :: #cert{}
	 }).

%%% module API

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).


-spec(send(#apns_msg{}, device_token()) -> ok | {error, too_big}).
send(#apns_msg{} = Msg, DeviceToken) ->
    gen_server:call(?MODULE, {send, Msg, DeviceToken}).


-spec(send_alert(iolist(), device_token()) -> ok | {error, too_big}).
send_alert(Alert, DeviceToken) ->
    send(#apns_msg{alert = Alert}, DeviceToken).


-spec(send_badge(integer(), device_token()) -> ok | {error, too_big}).
send_badge(Badge, DeviceToken) ->
    send(#apns_msg{badge = Badge}, DeviceToken).


-spec(send_data(iolist(), device_token()) -> ok | {error, too_big}).
send_data(Data, DeviceToken) ->
    send(#apns_msg{data = Data}, DeviceToken).


test_send() ->
    send(dp_push_apns:test_msg(), dp_push_apns:test_device_token()).


test_feedback() ->
    ?MODULE ! get_feedback.


%%% gen_server API

init({#apns{} = Apns, #cert{} = Cert}) ->
    ?INFO("~p inited with options ~p ~p ~n", [?MODULE, Apns, Cert]),
    {ok, #state{apns = Apns, cert = Cert}}.


handle_call({send, Msg, DeviceToken}, _From, #state{apns = Apns, cert = Cert} = State) ->
    Res = dp_push_apns:send(Msg, DeviceToken, Apns, Cert),
    {reply, Res, State};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(get_feedback, #state{apns = Apns, cert = Cert} = State) ->
    dp_push_apns:get_feedback(Apns, Cert),
    {noreply, State};

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	


