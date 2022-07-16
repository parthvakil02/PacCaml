open Csv
(**A level consists of a map filled with pathable tiles.*)

type tile_id = int

type tile = {
  id : tile_id;
  food : int;
  passible : bool;
}

let passibility tile = tile.passible

let food tile = tile.food

let get_id tile = tile.id

type map = {
  lst : tile array;
  (*assume maps are square for now*)
  num_cols : int;
}

let get_maplist map = map.lst

let get_numcols map = map.num_cols

exception UnknownTile of tile_id

let get_tile map tile_id = Array.get (get_maplist map) tile_id

(*translates the csv (which outputs as a string list list), to a string
  list*)
let rec append_csv_list a =
  match a with
  | [] -> []
  | h :: t -> append_csv_list t @ h

(*given string list from append_csv, makes each of the tiles based on
  their passibility, incrememnt id*)
let rec build_map lst id_num =
  match lst with
  | [] -> []
  | h :: t ->
      {
        id = id_num;
        food = (if h = "1" || h = "2" || h = "3" then 1 else 0);
        passible =
          (if h = "1" || h = "2" || h = "3" then true else false);
      }
      :: build_map t (id_num + 1)

let from_csv file_name =
  let a = file_name |> load in
  let b = append_csv_list a in
  ( { lst = Array.of_list (build_map b 0); num_cols = Csv.columns a },
    fst
      (List.fold_left
         (fun (counter, acc) x ->
           if x = "2" then (acc, acc + 1) else (counter, acc + 1))
         (0, 0) b),
    fst
      (List.fold_left
         (fun (counter, acc) x ->
           if x = "3" then (acc, acc + 1) else (counter, acc + 1))
         (0, 0) b),
    List.fold_left
      (fun acc x ->
        if x = "1" || x = "3" || x = "2" then acc + 1 else acc)
      0 (append_csv_list a)
    * 10
    - 10 )

let next_tile map tile direction =
  let tile_id = get_id tile in
  let next_tile =
    if direction = Command.Up then
      if tile_id + map.num_cols < map.num_cols * map.num_cols then
        tile_id + map.num_cols
      else tile_id mod map.num_cols
    else if direction = Command.Down then
      if tile_id - map.num_cols >= 0 then tile_id - map.num_cols
      else tile_id + (map.num_cols * (map.num_cols - 1))
    else if direction = Command.Right then
      if (tile_id + 1) mod map.num_cols != 0 then tile_id + 1
      else tile_id - (map.num_cols - 1)
    else if
      (tile_id - 1) mod map.num_cols != map.num_cols - 1 && tile_id > 0
    then tile_id - 1
    else tile_id + (map.num_cols - 1)
  in
  let a = get_tile map next_tile in
  if passibility a = false then tile else a

let eat_tile tile = { tile with food = 0 }

let rec find_eat_tile tile map_list =
  Array.set map_list (get_id tile) (eat_tile tile)

let eat_map map tile =
  let a = (food tile = 1, find_eat_tile tile (get_maplist map)) in
  (fst a, { num_cols = map.num_cols; lst = get_maplist map })

let try_direction map tile direction =
  if next_tile map tile direction = tile then false else true
