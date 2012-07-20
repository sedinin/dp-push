-type(device_token() :: integer()).

-type(apns_msg() :: binary()).

-record(apns, {
	  host :: string(),
	  port :: integer()
	 }).

-record(cert, {
	  certfile :: string(),
	  password :: string()
	 }).
