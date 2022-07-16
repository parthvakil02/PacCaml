open Graphics

type orientation =
  | Up
  | Down
  | Left
  | Right

type command =
  | On of orientation
  | Off
  | Quit

let translate_move st =
  if st.keypressed then
    match st.key with
    | 'a' -> On Left
    | 'w' -> On Up
    | 's' -> On Down
    | 'd' -> On Right
    | 'q' -> Quit
    | _ -> Off
  else Off

let opposite cmd =
  if cmd= Left then Right
  else if cmd=Right then Left
  else if cmd=Up then Down
  else Up
