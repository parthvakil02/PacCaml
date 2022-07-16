(**The Mapbuilder module is the module which allows users to create and
   save new playable maps*)

val main : int -> int -> unit
(**[main x y] runs the series of code that allows a user to make and
   save a valid map using a graphic interface (maps are valid if there
   is a ghoststart tile, hometile, and all tiles are reachable)*)
