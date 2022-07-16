(**The Ghosts module is a module which determines what directions
   respective ghosts go *)

exception NoGhost

val next_ghost_direction :
  Levels.map -> State.pacState -> Command.command
(** [nextGhostDirection map ghost_st] returns a valid next command for a
    ghost_st, by shuffling thorugh the directions that ghost can travel
    in.*)

val choose_ghost_direction :
  Levels.map -> State.pacState -> Command.command
(** [nextGhostDirection map ghost_st] returns a valid next command for a
    ghost_st, by shuffling thorugh the directions that ghost can travel
    in.*)

val random_ghost : State.t -> string -> Command.command
(**[random_ghost st schema] returns a directrion of a (usually) valid
   movement that ttries to maintain its current direction or picks a
   random direction if need be *)

val find_route :
  Levels.map ->
  Levels.tile ->
  Levels.tile ->
  bool array ->
  Command.command list ->
  Command.command list
(** [find_route map start final visited stack] finds a route from
    [start] tile to [final] tile in [map] and returns a list of Commands
    to get there. Requires: [visited] is initialized as an array the
    same size as [map] with every element false. [stack] is initialized
    as an empty list. *)

val to_tile_movement : State.t -> Levels.tile -> Command.command list
(** [to_tile_movement st tile] returns a list of Commands to get to a
    random tile in the map in [st] from start tile [tile]. Tiles are
    randomized with a state to avoid repetitive movement.*)

val chase_paccaml : State.t -> Levels.tile -> Command.command list
(** [chase_paccaml st tile] returns a list of Commands to get to the
    tile that paccaml is on in [st] from [tile].*)
