(** This module represents a level. It handles loading of that data from
    csv as well as querying the data to get more information*)

type map
(** The abstract type of values representing collection of tiles.
    Levels. A map is composed of a list of all tiles in their grid and a
    number of columns the grid is composed of (assume square). Tile Id's
    start at 0 and go to the last value*)

type tile_id = int
(** tile_id is an int representing a tiles position in a map. Tile id's
    are unique to tiles. *)

type tile
(** The abstract type of values representing tiles. Tile consists of a
    record that stores its food, passibility, and tile_id*)

val passibility : tile -> bool
(** [passibility tile] return the passability of a tile. i.e. true if
    paccaml can walk on the tile. false if it is impassible*)

val food : tile -> int
(** [food tile] return food on the tile*)

val get_id : tile -> tile_id
(* [get_id tile] returns id from a tile*)

val get_tile : map -> tile_id -> tile
(** [get_tile map tile] returns a tile given a tile_id and a map using
    List.find. Assumes all tile ID's are unique*)

val get_maplist : map -> tile array
(** [get_maplist map] return maplist given map*)

val get_numcols : map -> int
(** [get_numcols map] returns the number of columns in a map, given the
    map*)

exception UnknownTile of tile_id
(** Raised when an unknown tile is encountered. *)

val from_csv : string -> map * tile_id * tile_id * int
(** [from_csv a] returns the map that [a] represents as well as teh
    number of points in a map (number of edible tiles) and the starting
    tile which is indicated by a 2 on the mapdoc Requires: [a] is a
    valid csv map representation (a square csv consisting of 1's and
    0's). *)

val eat_map : map -> tile -> bool * map
(** [eat_map map tile] removes the food (dot for time being) from a tile
    if it is present for the tile which PacCaml is standing on. Finds
    tile in map and changes food to 0*)

val next_tile : map -> tile -> Command.orientation -> tile
(** [next_tile map tile_id direction] returns the next tile in the given
    direction from the given tile. Requires: given tile is a valid tile
    on the map. If the next tile is not passible or is not a valid tile,
    will instead return the current tile*)

val try_direction : map -> tile -> Command.orientation -> bool
(**[try_direction map tile_id direction] returns true if the next tile
   given a direction and a map is passible otherwise false*)
