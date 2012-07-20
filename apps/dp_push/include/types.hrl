-type(socket() :: pid()).

-record(apns, {
	  host :: string(),
	  port :: integer()
	 }).

-record(cert, {
	  certfile :: string(),
	  password :: string()
	 }).
