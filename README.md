Erlang library for working with Apple Push Notification Service from [dieselpuppet.com](http://dieselpuppet.com/).

[Apple Push Notification Service documentation](http://developer.apple.com/library/mac/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/ApplePushService/ApplePushService.html#//apple_ref/doc/uid/TP40008194-CH100-SW9).

[Great tutorial about using APNs](http://www.raywenderlich.com/3443/apple-push-notification-services-tutorial-part-12).

[dp_push documentation in russian](http://yzh44yzh.metalkia.com/post/128).


## How to use

Set dependency in your **rebar.config**:

	{deps, [
		{dp_push, ".*", {git, "https://github.com/yzh44yzh/dp-push.git", "v1.0"}}
	]}.

Change configuration in **dp_push.app.src**:

	{env, [{apns, [
			{host, "gateway.sandbox.push.apple.com"},
			{feedback_host, "feedback.sandbox.push.apple.com"},
			{port, 2195},
			{feedback_port, 2196}]
		},
		{cert, [{certfile, "priv/your_cert.pem"},
			{password, "your_password"}]
		},
		{failed_tokens_dets, "priv/failed_tokens"},
		{feedback_check_interval, 10800000} % 3 hours
	]}

Run application:

	main() ->
		ssl:start(),
		application:start(dp_push),

Call API functions:

	-spec(send(#apns_msg{}, device_token()) -> ok | {error, error()}).
	-spec(send_alert(iolist(), device_token()) -> ok | {error, error()}).
	-spec(send_badge(integer(), device_token()) -> ok | {error, error()}).
	-spec(send_data(iolist(), device_token()) -> ok | {error, error()}).
	-spec(remove_device_from_failed(device_token()) -> ok).


## Device Token

Device Token is something like that:

	8253de13 f71d310d 05a13135 e09e09b6 32c478d5 32313723 1f04a7c7 b5de947d

This is not a string or binary:

	InvalidDeviceToken = "8253de13 f71d310d 05a13135 e09e09b6 32c478d5 32313723 1f04a7c7 b5de947d"
	InvalidDeviceToken = "8253de13f71d310d05a13135e09e09b632c478d5323137231f04a7c7b5de947d"
	InvalidDeviceToken = <<"8253de13f71d310d05a13135e09e09b632c478d5323137231f04a7c7b5de947d">>

This is hex-number:

	ValidDeviceToken = 16#8253de13f71d310d05a13135e09e09b632c478d5323137231f04a7c7b5de947d

