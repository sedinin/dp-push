-module(apns).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([start/0, send/0, send/1]).

%%% module API
start() ->
    ssl:start().


send() ->
    Msg = "{\"aps\":{\"alert\":\"Hello from Erlang\",\"sound\":\"default\"}}",
    %% Msg = "{\"aps\":{\"alert\":\"You have mail!\"}}",
    send(list_to_binary(Msg)).


send(Msg) when is_binary(Msg) ->
    Host = "gateway.sandbox.push.apple.com",
    Port = 2195,
    Options = [
	       {certfile,"ck.pem"},
	       %% {certfile,"cert.pem"},
	       %% {keyfile, "key.pem"},
	       {password, "DieselPuppetPush01"}
	      ],
    io:format("send ~p to ~p ~p ~n", [Msg, Host, Port]),
    {ok, Socket} = ssl:connect(Host, Port, Options),
    io:format("connected: ~p~n", [Socket]),
    ok = ssl:send(Socket, pack(Msg)),
    io:format("send ~p ~n", [pack(Msg)]),
    receive
	Data -> io:format("client received: ~p~n", [Data])
    after 5000 -> io:format("timeout~n")
    end,
    io:format("done~n"),
    ssl:close(Socket),
    ok.


pack(Msg) when is_binary(Msg) ->
    DeviceToken = 16#9253de12f71d300d05a11135e09e09b632c478d5323137231f04a7c7b4de947d,
    Size = byte_size(Msg),
    %% Id = 1,
    %% Expire = 100,
    %% <<1,Id:32/integer,Expire:32/integer,0,32,DeviceToken/integer,Size:16/integer,Msg/binary>>.
    <<0,0,32,DeviceToken:256/integer,Size:16/integer,Msg/binary>>.

