-module(apns).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([send/4, wrap_to_json/1]).
-export([test_device_token/0, test_msg/0]).
-include("logger.hrl").
-include("types.hrl").

%%% module API

-spec(send(#apns_msg{}, device_token(), #apns{}, #cert{}) -> ok).
send(#apns_msg{} = Msg, DeviceToken, #apns{host = Host, port = Port},
     #cert{certfile = Certfile, password = Password}) ->
    {ok, Socket} = ssl:connect(Host, Port, [{certfile, Certfile}, {password, Password}]),
    ok = ssl:send(Socket, pack_simple(wrap_to_json(Msg), DeviceToken)),
    ssl:close(Socket),
    ok.


-spec(wrap_to_json(#apns_msg{}) -> binary()).
wrap_to_json(#apns_msg{alert = Alert, badge = Badge, sound = Sound, data = Data}) ->
    list_to_binary(
      ["{\"aps\":{",
       merge_attr(wrap_alert(Alert),
		  wrap_badge(Badge),
		  wrap_sound(Sound)),
       "}",
       wrap_data(Data),
       "}"]).


merge_attr([], [], []) -> [];
merge_attr(L1, [], []) -> L1;
merge_attr([], L2, []) -> L2;
merge_attr([], [], L3) -> L3;
merge_attr(L1, [], L3) -> [L1, ",", L3];
merge_attr(L1, L2, []) -> [L1, ",", L2];
merge_attr([], L2, L3) -> [L2, ",", L3];
merge_attr(L1, L2, L3) -> [L1, ",", L2, ",", L3].

wrap_alert(#alert{body = Body}) -> wrap_alert(Body); % TODO wrap all fields of #alert record
wrap_alert(undefined) -> [];
wrap_alert([]) -> [];
wrap_alert(Alert) -> ["\"alert\":\"", Alert, "\""].

wrap_badge(undefined) -> [];
wrap_badge(Num) -> ["\"badge\":\"", integer_to_list(Num), "\""].

wrap_sound(undefined) -> [];
wrap_sound(default) -> ["\"sound\":\"default\""];
wrap_sound(Sound) -> ["\"sound\":\"", Sound, "\""].

wrap_data(undefined) -> [];
wrap_data(Data) -> [",\"d\":\"", Data, "\""].


-spec(pack_simple(binary(), device_token()) -> binary()).
pack_simple(Msg, DeviceToken) ->
    Size = byte_size(Msg),
    <<0,0,32,DeviceToken:256/integer,Size:16/integer,Msg/binary>>.


%% -spec(pack_enhanced(binary(), device_token(), integer(), integer()) -> binary()).
%% pack_enhanced(Msg, DeviceToken, Id, Expire) ->
%%     Size = byte_size(Msg),
%%     <<1,Id:32/integer,Expire:32/integer,0,32,DeviceToken/integer,Size:16/integer,Msg/binary>>.


-spec(test_device_token() -> device_token()).
test_device_token() ->
    16#9253de12f71d300d05a11135e09e09b632c478d5323137231f04a7c7b4de947d.


-spec(test_msg() -> #apns_msg{}).
test_msg() ->
    #apns_msg{alert = "Hello from Erlang", sound = default}.
