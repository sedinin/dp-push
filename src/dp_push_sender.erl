-module(dp_push_sender).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/1, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([test_send/0, test_feedback/0]).
-include("logger.hrl").
-include("types.hrl").

-record(state, {
	  table_id :: integer(),
	  apns :: #apns{},
	  cert :: #cert{}
	 }).

%%% module API

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).


-spec(send(#apns_msg{}, device_token()) -> ok | {error, error()}).
send(#apns_msg{} = Msg, DeviceToken) ->
    gen_server:call(?MODULE, {send, Msg, DeviceToken}).


test_send() ->
    send(dp_push_apns:test_msg(), dp_push_apns:test_device_token()).


test_feedback() ->
    ?MODULE ! get_feedback.


%%% gen_server API

init({DetsFile, #apns{} = Apns, #cert{} = Cert}) ->
    ?INFO("~p inited with options ~p ~p ~n", [?MODULE, Apns, Cert]),
    {ok, TableId} = dets:open_file(failed_tokens,
				   [{type, set}, {file, DetsFile}]),
    ?INFO("table info ~p~n", [dets:info(TableId)]),
    {ok, #state{table_id = TableId, apns = Apns, cert = Cert}}.


handle_call({send, Msg, DeviceToken}, _From, #state{apns = Apns, cert = Cert} = State) ->
    Res = dp_push_apns:send(Msg, DeviceToken, Apns, Cert),
    {reply, Res, State};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(get_feedback, #state{table_id = TableId, apns = Apns, cert = Cert} = State) ->
    case dp_push_apns:get_feedback(Apns, Cert) of
	{ok, Tokens} -> ?INFO("tokens from feedback ~p~n", [Tokens]),
			save_tokens(TableId, Tokens);
	{error, _} -> do_nothig
    end,
    {noreply, State};

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, #state{table_id = TableId}) ->
    dets:close(TableId),
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	


save_tokens(_TableId, []) -> ok;

save_tokens(TableId, [Token|Tokens]) ->
    case dets:lookup(TableId, Token) of
	[{Token, Count}] -> dets:insert(TableId, {Token, Count + 1});
	[] -> dets:insert(TableId, {Token, 1})
    end,
    save_tokens(TableId, Tokens).
    
			       
