(**The history module is the module which acts on the history file which
   is created locally to track high score and maximum number of points*)

val init : unit -> int * int
(**[init ()] initialize the history file. If there is history of
   playing, load it in. Otherwise, make the file. first int is lifetime
   points, second is highscore*)

val save : int -> unit
(**[save points] takes a number of points and updates history as a
   result. First element of history is the lifetime points. second is
   the high score*)
