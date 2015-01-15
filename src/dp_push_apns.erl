-module(dp_push_apns).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([send/4, get_feedback/2, wrap_to_json/1]).

-include("logger.hrl").
-include("dp_push_types.hrl").

%%% module API

-spec(send(#apns_msg{}, device_token(), #apns{}, #cert{}) ->
	     ok | {error, too_big, integer()} | {error, term()}).
send(#apns_msg{} = Msg, DeviceToken, #apns{host = Host, port = Port},
     #cert{certfile = Certfile, password = Password}) ->
    Json = wrap_to_json(Msg),
    case byte_size(Json) of
	Len when Len > 255 -> {error, too_big, Len};
	_ -> case ssl:connect(Host, Port, [{certfile, Certfile}, {password, Password}]) of
		 {ok, Socket} -> ok = ssl:send(Socket, pack_simple(Json, DeviceToken)),
				 ssl:close(Socket),
				 ok;
		 {error, Error} -> ?ERROR("can't connect to ~p:~p ~p~n", [Host, Port, Error]),
				   {error, Error}
	     end
    end.


-spec(get_feedback(#apns{}, #cert{}) -> {ok, [device_token()]} | {error, term()}).
get_feedback(#apns{feedback_host = Host, feedback_port = Port},
	     #cert{certfile = Certfile, password = Password}) ->
    case ssl:connect(Host, Port, [{certfile, Certfile}, {password, Password}]) of
	{ok, Socket} -> Data = read_feedback([]),
			Tokens = get_tokens(Data, []),
			ssl:close(Socket),
			{ok, Tokens};
	{error, Error} ->?ERROR("can't connect to ~p:~p ~p~n", [Host, Port, Error]),
			 {error, Error}
    end.

    
read_feedback(Data) ->
    receive
	{ssl, _, Part} -> read_feedback([Part|Data]);
	{ssl_closed, _} -> list_to_binary(lists:reverse(Data))
    end.


get_tokens(<<>>, Tokens) -> Tokens;

get_tokens(Data, Tokens) ->
    case Data of
	<<_Time:32, Size:16/integer, Left1/binary>> ->
	    BSize = Size * 8,
	    case Left1 of
		<<Token:BSize/integer, Left2/binary>> ->
		    get_tokens(Left2, [Token|Tokens]);
		_ -> Tokens
	    end;
	_ -> Tokens
    end.
    

-spec(wrap_to_json(#apns_msg{}) -> binary()).
wrap_to_json(#apns_msg{alert = Alert, badge = Badge, sound = Sound, content_available = CA, data = Data}) ->
    list_to_binary(
      ["{\"aps\":{",
       merge_attr([wrap_alert(Alert),
                   wrap_badge(Badge),
                   wrap_sound(Sound),
                   wrap_content_available(CA)]),
       "}",
       wrap_data(Data),
       "}"]).


merge_attr(Elems) ->
        merge_attrs(lists:filter(fun(E) -> E =/= [] end, Elems), []).
merge_attrs([], [E]) -> E;
merge_attrs([], Acc) -> lists:reverse(Acc);
merge_attrs([E], Acc) -> merge_attrs([], [E | Acc]);
merge_attrs([E|T], Acc) -> merge_attrs(T, [",", E | Acc]).

wrap_alert(#alert{body = Body}) -> wrap_alert(Body); % TODO wrap all fields of #alert record
wrap_alert(undefined) -> [];
wrap_alert([]) -> [];
wrap_alert(Alert) -> ["\"alert\":\"", Alert, "\""].

wrap_badge(undefined) -> [];
wrap_badge(Num) -> ["\"badge\":", integer_to_list(Num)].

wrap_sound(undefined) -> [];
wrap_sound(default) -> ["\"sound\":\"default\""];
wrap_sound(Sound) -> ["\"sound\":\"", Sound, "\""].

wrap_content_available(undefined) -> [];
wrap_content_available(Num) -> ["\"content-available\":", integer_to_list(Num)].

wrap_data(undefined) -> [];
wrap_data([]) -> [];
wrap_data([{K, V}|T]) when is_binary(K), is_binary(V) ->
    [[",\"", K, "\":\"", V, "\""] | wrap_data(T)];
wrap_data(Data) -> [",\"d\":\"", Data, "\""].


-spec(pack_simple(binary(), device_token()) -> binary()).
pack_simple(Msg, DeviceToken) ->
    Size = byte_size(Msg),
    <<0,0,32,DeviceToken:256/integer,Size:16/integer,Msg/binary>>.
