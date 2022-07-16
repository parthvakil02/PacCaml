open Graphics

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

let current_tile st pac =
  match pac with
  | "javarition" -> st.ghosts.java.tile
  | "pythongeist" -> st.ghosts.python.tile
  | "cpirit" -> st.ghosts.cpirit.tile
  | "sqhell" -> st.ghosts.sqhell.tile
  | _ -> st.pacState.tile

let current_direction st pac =
  match pac with
  | "javarition" -> st.ghosts.java.orientation
  | "pythongeist" -> st.ghosts.python.orientation
  | "cpirit" -> st.ghosts.cpirit.orientation
  | "sqhell" -> st.ghosts.sqhell.orientation
  | _ -> st.pacState.orientation

let go cmd st pac =
  (*this is my favorite function that we have lol*)
  match cmd with
  | Command.On h ->
      {
        orientation = h;
        tile = Levels.next_tile st.map (current_tile st pac) h;
      }
  | _ ->
      {
        orientation = current_direction st pac;
        tile = current_tile st pac;
      }

let eat_map_tile state tile = Levels.eat_map state.map tile

let check_collision st last_state =
  let caml = Levels.get_id st.pacState.tile in
  let last_caml = Levels.get_id last_state.pacState.tile in
  caml = Levels.get_id st.ghosts.java.tile
  || caml = Levels.get_id st.ghosts.python.tile
  || caml = Levels.get_id st.ghosts.cpirit.tile
  || caml = Levels.get_id st.ghosts.sqhell.tile
  || last_caml = Levels.get_id st.ghosts.sqhell.tile
     && caml = Levels.get_id last_state.ghosts.sqhell.tile
  || last_caml = Levels.get_id st.ghosts.python.tile
     && caml = Levels.get_id last_state.ghosts.python.tile
  || last_caml = Levels.get_id st.ghosts.cpirit.tile
     && caml = Levels.get_id last_state.ghosts.cpirit.tile
  || last_caml = Levels.get_id st.ghosts.java.tile
     && caml = Levels.get_id last_state.ghosts.java.tile

let identify_collision st last_state =
  (*this one returns who the collision is with, instead of a bool like
    in check collision*)
  let caml = Levels.get_id st.pacState.tile in
  let last_caml = Levels.get_id last_state.pacState.tile in
  if
    caml = Levels.get_id st.ghosts.java.tile
    || last_caml = Levels.get_id st.ghosts.java.tile
       && caml = Levels.get_id last_state.ghosts.java.tile
  then "java"
  else if
    caml = Levels.get_id st.ghosts.python.tile
    || last_caml = Levels.get_id st.ghosts.python.tile
       && caml = Levels.get_id last_state.ghosts.python.tile
  then "python"
  else if
    caml = Levels.get_id st.ghosts.cpirit.tile
    || last_caml = Levels.get_id st.ghosts.cpirit.tile
       && caml = Levels.get_id last_state.ghosts.cpirit.tile
  then "cpirit"
  else "sqhell"

let eat_bonus_tile bonustile st =
  Levels.get_id st.pacState.tile = bonustile

let eat_power_tile powertiles st =
  let pactile = Levels.get_id st.pacState.tile in
  if List.mem pactile powertiles then
    ( List.filter
        (fun x -> x <> Levels.get_id st.pacState.tile)
        powertiles,
      true,
      pactile )
  else (powertiles, false, 0)

let rec find_rand_tile numcols map =
  (*randomizes two numbers in the bounds of a map. If that tile is
    passible, then return that tile. Otherwise, try again*)
  let num1 = Random.int (numcols * numcols / 2 mod 230) in
  let num2 = Random.int (numcols * numcols / 2 mod 230) in
  let try_tile =
    Levels.get_tile map ((num1 + num2) mod ((numcols * numcols) - 1))
  in
  if Levels.passibility try_tile then Levels.get_id try_tile
  else find_rand_tile numcols map

let rec make_tile_list num_tiles map numcols =
  (*helper function which appends a set number of tiles onto alist of
    other valid tiles*)
  match num_tiles with
  | a when a = 0 -> []
  | a ->
      find_rand_tile numcols map :: make_tile_list (a - 1) map numcols

let make_power_tiles map =
  let num_tiles = Levels.get_numcols map / 5 in
  make_tile_list num_tiles map (Levels.get_numcols map)

let update_bonus_tile st ctr starttile bonustile =
  match bonustile with
  | None when st.points mod 500 = 0 && ctr > 30 && st.points > 0 ->
      (Some starttile, 0, false)
  | None -> (None, ctr, false)
  | Some a when eat_bonus_tile a st -> (None, 0, true)
  | Some a when ctr > 35 -> (None, ctr, false)
  | Some a -> (Some a, ctr, false)

let rec disp_bonus_helper displist =
  (*[disp_bonus_helper displist] takes a displist and reduces the
    turnsleft (second) component of each of the components. If they get
    to zero then return*)
  match displist with
  | [] -> []
  | (loc, turnsleft, points) :: t ->
      if turnsleft = 0 then disp_bonus_helper t
      else (loc, turnsleft - 1, points) :: disp_bonus_helper t

let rec disp_bonus_maintain
    displist
    power
    power_tile
    bonus_eat
    levelnum
    starttile
    eat
    eattile =
  let temp_list = disp_bonus_helper displist in
  let temp_list2 =
    if power then (power_tile, 4, 50) :: temp_list else temp_list
  in
  let templist3 =
    if bonus_eat then (starttile, 4, levelnum * 100) :: temp_list2
    else temp_list2
  in
  if eat then (eattile, 4, 200) :: templist3 else templist3
