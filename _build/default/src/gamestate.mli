(**The Gamestate module is a module which handles functions that operate
   on state*)

type const = {
  x : int;
  y : int;
  starttile : int;
  startghosttile : int;
  maxpoints : int;
  speed : float;
  mapname : string;
  levelnum : int;
}

val update_state :
  bool * Levels.map ->
  State.pacState ->
  bool ->
  State.t ->
  Levels.tile ->
  State.t ->
  bool ->
  int ->
  bool ->
  State.t
(**[update_state maptuple next_tile st ghosttile lasttile bonus_eat levelnum]
   returns the state given the next place it is moving to, to be used in
   update in main*)

val die_state : State.t -> int -> int -> State.t
(**[die_state map points lives starttile startghost tile] gives an state
   with all the current progress [st], ghosts on [startghosttile]
   pointing in various directions, paccaml starting on [starttile]
   facing right, and decrements lives by 1*)

val init_state : Levels.map -> int -> int -> int -> int -> State.t
(**[init_state map points lives starttile startghost tile] gives an
   initial state with map [map], ghosts on [startghosttile] pointing in
   various directions, paccaml starting on [starttile] facing right,
   [lives] and [points]*)

val move_queue :
  State.t -> Command.command -> Command.command * Command.command
(**[move_queue state queue] returns a tuple corresponding to both the
   move_direction (using go) and the queued direction. This implements
   wait_next_event and user input*)
