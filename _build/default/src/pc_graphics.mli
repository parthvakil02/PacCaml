(**The Pc_graphics module is the module which handles creating graphics
   based off the datatypes created elsewhere*)

val welcome_screen : int * int -> int -> int -> unit
(** [welcome_screen life_points x y] is a function that takes in pixel
    sizes x and y and opens a window with a Welcome Screen of x by y
    dimensions *)

val update_screen : int -> int -> bool
(** [update x y] is a function that takes in pixel sizes x and y and
    opens a window with a update Screen of x by y dimensions *)

val you_lose_screen : int -> int -> unit
(** [you_lose_screen x y] is a function that takes in pixel sizes x and
    y and opens a window with a you_lose_screen of x by y dimensions *)

val death_screen : int -> int -> bool
(** [death_screen x y] is a function that takes in pixel sizes x and y
    and opens a window with a death_screen of x by y dimensions. if you
    hit q it returns false. otherwise returns true *)

val build_pac :
  Levels.tile -> int -> int -> string -> string -> bool -> unit
(**[build_caml tile numcols x img schema mode] draws img of pac
   character indicated by schema on a particular tile. [mode] is a bool
   indicating if the game is in power mode or not.*)

val pac_image : Command.command -> Command.command -> int -> string
(**[pac_image newdir olddir ctr] returns the data file for paccaml to be
   drawn based on the new directions (unless that isnt valid, then it
   tries the new direction)*)

val ghost_image : int -> string -> string
(** [ghost_image ctr ghost] returns the data file for the ghost to be
    drawn as indicated by ghost based on the new directions (unless that
    isnt valid, then it tries the new direction). Requires: ghost is
    either "javarition", "pythongeist", or .... If none are selected it
    returns an empty string.*)

val draw_sprites :
  State.t ->
  int ->
  Command.command ->
  Command.command ->
  int ->
  bool ->
  unit
(**[draw_sprites st x move queue ctr mode] draws the various sprites (4
   ghosts and paccaml) onto the tile they are going onto with the
   correct image*)

val update_graphics :
  State.t ->
  int option ->
  int list ->
  Gamestate.const ->
  (int * int * int) list ->
  unit
(**[update_graphics st x bonustile powertiles disp_bon] draws the maps
   and updates all the text at the top of the graphics window to reflect
   the current state*)
