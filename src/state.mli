(**The State module is the module which handles the state of paccaml,
   which includes the map, paccamls location, the ghosts location and
   direction, points, bonus points, and lives*)

type pacState = {
  tile : Levels.tile;
  orientation : Command.orientation;
}

type ghosts = {
  java : pacState;
  python : pacState;
  cpirit : pacState;
  sqhell : pacState;
}

type t = {
  map : Levels.map;
  pacState : pacState;
  ghosts : ghosts;
  points : int;
  lives : int;
  bonuspoints : int;
}

val current_tile : t -> string -> Levels.tile
(**[current_tile st schema] returns the tile of a given sprite, with the
   schema being the name of the ghost or caml you want the direction of*)

val current_direction : t -> string -> Command.orientation
(**[current_direction st schema] returns the direction of a given
   sprite, with the schema being the name of the ghost or caml you want
   the direction of*)

val check_collision : t -> t -> bool
(**[check_collisions st last_state] returns true if paccaml and a ghost
   are on the same tile or pass eacheachother through the same tile.*)

val identify_collision : t -> t -> string
(**[indentify_collsiion st last_state] returns the name of the ghost
   that there was a collision with... requires: a collision has already
   happened*)

val go : Command.command -> t -> string -> pacState
(**[go dir state pac] returns a pacState corresponding to moving in the
   direction given. If direction is not a viable direction, location
   does not change but orientation changes. [pac] determines if paccaml
   or one of the ghosts will move.

   Requires: pac is either "pythongeist", "javarition", or "caml".*)

val eat_map_tile : t -> Levels.tile -> bool * Levels.map
(**[eat_map_tile state tile] returns a tuple which fst value is a
   boolean, did this function eat anything, and the second value is the
   map corresponding to that one tile eaten*)

val eat_bonus_tile : int -> t -> bool
(**[eat_bonus_tile bonustile state] returns true if paccaml are on a
   bonus tile*)

val eat_power_tile :
  Levels.tile_id list -> t -> Levels.tile_id list * bool * int
(**[eat_power_tile powertiles st] return true if you are standing on a
   pragmatic tile. also returns an updated tile list and the id of the
   tile u were standing on*)

val make_power_tiles : Levels.map -> Levels.tile_id list
(**[make_power_tiles map st] returns a set of powertiles with the number
   of them given by the size of the map*)

val disp_bonus_maintain :
  (int * int * int) list ->
  bool ->
  int ->
  bool ->
  int ->
  int ->
  bool ->
  int ->
  (int * int * int) list
(**[disp_bonus_maintain displist power bonus_eat levelnum starttile]
   returns an updated list which contains the (loc, turns_left, points)
   which is all the necessary info to track what points text should be
   displayed*)

val update_bonus_tile :
  t -> int -> int -> int option -> int option * int * bool
(**[update_bonus_tile st ctr starttile bonustile] returns a triplet of a
   new bonus tile (depending on if it is time to make a bonustile), a
   new ctr, and a bool describing if the bonus tile was eaten *)
