open Graphics
(**Translating user inputs, keystrokes*)

(**The Command module represents whether a user input causes a movement
   or not, and handles interfacing the user input to commands*)

type orientation =
  | Up
  | Down
  | Left
  | Right

type command =
  | On of orientation
  | Off
  | Quit

val translate_move : status -> command
(**[translate_move input] checks [status] to see if a key is pressed. If
   a valid key has been pressed, evaluates to On. If a key has not been
   pressed or that key is invalid, it evaluates to Off.*)

val opposite : orientation -> orientation
(**[opposite dir] returns the opposite of the given direction (i.e.
   north returns south)*)
