-ifndef(ERROR_TYPE).
-define(ERROR_TYPE, true).
-type(error() :: term()).
-endif.

-ifndef(DEVICE_TOKEN_TYPE).
-define(DEVICE_TOKEN_TYPE, true).
-type(device_token() :: integer()).
-endif.

-record(apns, {
	  host :: string(),
	  port :: integer(),
	  feedback_host :: string(),
	  feedback_port :: integer()
	 }).

-record(cert, {
	  certfile :: string(),
	  password :: string()
	 }).

-record(alert, {
	  body :: iolist(),
	  action_loc_key :: iolist() | null,
	  loc_key :: iolist(),
	  loc_args :: [iolist()],
	  launch_image :: iolist()
	 }).

-record(apns_msg, {
	  alert :: iolist() | #alert{}, % An alert message to display to the user
	  badge :: integer(),           % A number to badge the application icon with
	  sound :: iolist() | default,  % A sound to play
	  data :: iolist()              % custom payload values outside the Apple-reserved aps namespace
	 }).



