-module(apns).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([send/3, pack_simple/2, pack_enhanced/4]).
-export([test_device_token/0, test_msg/0]).
-include("types.hrl").


%%% module API

-spec(send(apns_msg(), #apns{}, #cert{}) -> ok).
send(Msg, #apns{host = Host, port = Port},
     #cert{certfile = Certfile, password = Password}) ->
    {ok, Socket} = ssl:connect(Host, Port, [{certfile, Certfile}, {password, Password}]),
    ok = ssl:send(Socket, Msg),
    ssl:close(Socket),
    ok.


-spec(pack_simple(binary(), device_token()) -> apns_msg()).
pack_simple(Msg, DeviceToken) ->
    Size = byte_size(Msg),
    <<0,0,32,DeviceToken:256/integer,Size:16/integer,Msg/binary>>.


-spec(pack_enhanced(binary(), device_token(), integer(), integer()) -> apns_msg()).
pack_enhanced(Msg, DeviceToken, Id, Expire) ->
    Size = byte_size(Msg),
    <<1,Id:32/integer,Expire:32/integer,0,32,DeviceToken/integer,Size:16/integer,Msg/binary>>.


-spec(test_device_token() -> device_token()).
test_device_token() ->
    16#9253de12f71d300d05a11135e09e09b632c478d5323137231f04a7c7b4de947d.


-spec(test_msg() -> binary()).
test_msg() ->
    list_to_binary("{\"aps\":{\"alert\":\"Hello from Erlang\",\"sound\":\"default\"}}").
