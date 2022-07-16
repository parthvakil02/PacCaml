(*doing stuff with the ghost bois*)
open Command
open State
open Random

exception NoGhost

let distance tile pactile numcols =
  ( (tile mod numcols) - (pactile mod numcols),
    (tile / numcols) - (pactile / numcols) )

let next_ghost_direction map ghost_St =
  match Levels.next_tile map ghost_St.tile Up with
  | tile -> (
      if tile != ghost_St.tile then On Up
      else
        match Levels.next_tile map ghost_St.tile Command.Right with
        | tile -> (
            if tile != ghost_St.tile then On Right
            else
              match Levels.next_tile map ghost_St.tile Command.Down with
              | tile -> (
                  if tile != ghost_St.tile then On Down
                  else
                    match
                      Levels.next_tile map ghost_St.tile Command.Left
                    with
                    | tile ->
                        if tile != ghost_St.tile then On Left else Off))
      )

let shuffle = List.sort (fun _ _ -> Random.int 3 - 1)

let rec ghost_direction_helper map ghost_St directions =
  match shuffle [ Up; Down; Left; Right ] with
  | [] -> Off
  | h :: t ->
      if Levels.next_tile map ghost_St.tile h != ghost_St.tile then On h
      else ghost_direction_helper map ghost_St t

let choose_ghost_direction map ghost_St =
  let directions = shuffle [ Up; Down; Left; Right ] in
  ghost_direction_helper map ghost_St directions

let check_dir map start dir visited =
  let dirtile = Levels.next_tile map start dir in
  let condit1 = Array.get visited (Levels.get_id dirtile) = false in
  let condit2 = dirtile <> start in
  (*if condit1 then let () = print_string "condit1" in condit1 &&
    condit2 else if condit2 then let () = print_string "condit2" in
    condit1 && condit2 else let () = print_string "neither" in*)
  condit1 && condit2

(*Taken from Stack module in textbook*)
exception Empty

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

let push = List.cons

let peek = function
  | [] -> raise Empty
  | x :: _ -> x

let pop = function
  | [] -> raise Empty
  | _ :: s -> s

let size = List.length

let to_list = Fun.id

let rec find_route map start final visited stack =
  if start = final then stack
  else (
    Array.set visited (Levels.get_id start) true;
    if check_dir map start Command.Left visited then
      (*let () = print_string "can go left" in*)
      find_route map
        (Levels.next_tile map start Command.Left)
        final visited
        (push (Command.On Left) stack)
    else if check_dir map start Command.Right visited then
      (*let () = print_string "can go right" in*)
      find_route map
        (Levels.next_tile map start Command.Right)
        final visited
        (push (Command.On Right) stack)
    else if check_dir map start Command.Up visited then
      (*let () = print_string "can go up" in*)
      find_route map
        (Levels.next_tile map start Command.Up)
        final visited
        (push (Command.On Up) stack)
    else if check_dir map start Command.Down visited then
      let () = print_string "can go down" in
      find_route map
        (Levels.next_tile map start Command.Down)
        final visited
        (push (Command.On Down) stack)
    else
      let () = print_string "popping" in
      pop stack)

let rec to_tile_movement st tile =
  Random.self_init ();
  let height = Levels.get_numcols st.map in
  let newtile = Random.int ((height * height) - 1) in
  if
    Levels.passibility (Levels.get_tile st.map newtile)
    && Levels.get_id tile <> newtile
  then
    find_route st.map tile
      (Levels.get_tile st.map newtile)
      (Array.init (height * height) (fun x -> false))
      []
  else to_tile_movement st tile

let chase_paccaml st tile =
  let height = Levels.get_numcols st.map in
  find_route st.map tile st.pacState.tile
    (Array.init (height * height) (fun x -> false))
    []

let rec pick_dir dir tile map =
  let a = Random.int 4 in
  let new_dir =
    if a = 0 then Left
    else if a = 1 then Right
    else if a = 2 then Up
    else Down
  in
  if Levels.try_direction map tile new_dir && new_dir <> opposite dir
  then On new_dir
  else pick_dir dir tile map

let random_ghost st schema =
  let ghost_tile = current_tile st schema in
  let ghost_dir = current_direction st schema in

  pick_dir ghost_dir ghost_tile st.map
