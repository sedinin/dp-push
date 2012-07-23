-type(device_token() :: integer()).


-record(apns, {
	  host :: string(),
	  port :: integer()
	 }).

-record(cert, {
	  certfile :: string(),
	  password :: string()
	 }).

-record(alert, {
	  body :: string(),
	  action_loc_key :: string() | null,
	  loc_key :: string(),
	  loc_args :: [string()],
	  launch_image :: string()
	 }).

-record(apns_msg, {
	  alert :: string() | #alert{}, % An alert message to display to the user
	  badge :: integer(),           % A number to badge the application icon with
	  sound :: string() | default,  % A sound to play
	  data :: string()              % custom payload values outside the Apple-reserved aps namespace
	 }).



