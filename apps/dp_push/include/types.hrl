-type(connection() :: pid()).
-type(socket() :: pid()).
-type(db_connection() :: pid()).
-type(db_worker() :: pid()).
-type(ets_tab_name() :: atom()).
-type(user_id() :: binary()).
-type(dices() :: [integer()]).

-record(user, {
	  id :: user_id(),
	  name :: binary(),
	  avatar :: binary(),
	  money = 0 :: integer()
	 }).

-record(session, {
	  connection :: connection(),
	  user :: #user{}
	 }).

-record(game_state, {
	  u1 :: user_id(),
	  u2 :: user_id(),
	  p1 = 0 :: integer(),
	  p2 = 0 :: integer(),
	  move_by :: user_id(),
	  dices = [] :: dices(),
	  num_rolls = 0 :: integer(),
	  hand1 = [] :: [hand()],
	  hand2 = [] :: [hand()]
	 }).

-type(room_pid() :: pid()).
-type(room_id() :: binary()).
-type(room_state() :: created | waiting | active).

-record(room, {
	  id :: room_id(),
	  state = created :: room_state(),
	  sessions = [] :: [#session{}],
	  game_state :: #game_state{}
	 }).

-type(hand() :: ones | twos | threes | fours | fives | sixs | two_pairs |
		three_of_kind | four_of_kind | full_house | small_straight |
		large_straight | five_of_kind | chance | unknown).


-define(TOTAL_HANDS, 14).
