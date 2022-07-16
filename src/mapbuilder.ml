open Graphics

let rec append_list a =
  match a with
  | [] -> []
  | h :: t -> append_list t @ h

(**[dfs_right_check map numcols tile visited] checks if there is valid
   (existing and not fixited) tile in the down direction*)
let dfs_right_check map numcols tile visited =
  (Array.get map
     (if (tile + 1) mod numcols != 0 then tile + 1
     else tile - numcols + 1)
   = "1"
  || Array.get map
       (if (tile + 1) mod numcols != 0 then tile + 1
       else tile - numcols + 1)
     = "2"
  || Array.get map
       (if (tile + 1) mod numcols != 0 then tile + 1
       else tile - numcols + 1)
     = "3")
  && Array.get visited
       (if (tile + 1) mod numcols != 0 then tile + 1
       else tile - numcols + 1)
     = 0

(**[dfs_left_check map numcols tile visited] checks if there is valid
   (existing and not fixited) tile in the down direction*)
let dfs_left_check map numcols tile visited =
  (Array.get map
     (if (tile - 1) mod numcols != numcols - 1 && tile > 0 then tile - 1
     else tile + numcols - 1)
   = "1"
  || Array.get map
       (if (tile - 1) mod numcols != numcols - 1 && tile > 0 then
        tile - 1
       else tile + numcols - 1)
     = "2"
  || Array.get map
       (if (tile - 1) mod numcols != numcols - 1 && tile > 0 then
        tile - 1
       else tile + numcols - 1)
     = "3")
  && Array.get visited
       (if (tile - 1) mod numcols != numcols - 1 && tile > 0 then
        tile - 1
       else tile + numcols - 1)
     = 0

(**[dfs_up_check map numcols tile visited] checks if there is valid
   (existing and not fixited) tile in the down direction*)
let dfs_up_check map numcols tile visited =
  (Array.get map
     (if tile + numcols < numcols * numcols then tile + numcols
     else tile mod numcols)
   = "1"
  || Array.get map
       (if tile + numcols < numcols * numcols then tile + numcols
       else tile mod numcols)
     = "2"
  || Array.get map
       (if tile + numcols < numcols * numcols then tile + numcols
       else tile mod numcols)
     = "3")
  && Array.get visited
       (if tile + numcols < numcols * numcols then tile + numcols
       else tile mod numcols)
     = 0

(**[dfs_down_check map numcols tile visited] checks if there is valid
   (existing and not fixited) tile in the down direction*)
let dfs_down_check map numcols tile visited =
  (Array.get map
     (if tile - numcols < 0 then (numcols * numcols) - numcols + tile
     else tile - numcols)
   = "1"
  || Array.get map
       (if tile - numcols < 0 then (numcols * numcols) - numcols + tile
       else tile - numcols)
     = "2"
  || Array.get map
       (if tile - numcols < 0 then (numcols * numcols) - numcols + tile
       else tile - numcols)
     = "3")
  && Array.get visited
       (if tile - numcols < 0 then (numcols * numcols) - numcols + tile
       else tile - numcols)
     = 0

(**[dfs map numcols tile visited] returns the number of tiles in the map
   that are pathable from the start tile. There are some complexities in
   implementation, takes a string array, start tile, number of colums,
   and empty visited array, and returns the number of tiles.*)
let rec dfs map numcols tile visited =
  Array.set visited tile 1;

  (if dfs_right_check map numcols tile visited then
   dfs map numcols
     (if (tile + 1) mod numcols != 0 then tile + 1
     else tile - numcols + 1)
     visited
   + 1
  else 0)
  + (if dfs_left_check map numcols tile visited then
     dfs map numcols
       (if (tile - 1) mod numcols != numcols - 1 && tile > 0 then
        tile - 1
       else tile + numcols - 1)
       visited
     + 1
    else 0)
  + (if dfs_up_check map numcols tile visited then
     dfs map numcols
       (if tile + numcols < numcols * numcols then tile + numcols
       else tile mod numcols)
       visited
     + 1
    else 0)
  +
  if dfs_down_check map numcols tile visited then
    dfs map numcols
      (if tile - numcols < 0 then (numcols * numcols) - numcols + tile
      else tile - numcols)
      visited
    + 1
  else 0

(**[check map map numcols] checks to make sure a given map is valid map.
   A valid map has all tiles reachable from the start tile.*)
let check_map (map : string list list) numcols =
  let maplist = append_list map in
  (*count how many points are in a map*)
  let num_tiles =
    List.fold_left
      (fun acc x ->
        if x = "1" || x = "3" || x = "2" then acc + 1 else acc)
      0 maplist
  in
  (*find the start tile --- to initiate the dfs from*)
  let starttile =
    fst
      (List.fold_left
         (fun (counter, acc) x ->
           if x = "2" then (acc, acc + 1) else (counter, acc + 1))
         (0, 0) maplist)
  in

  let map_array = Array.of_list maplist in

  (*compare the dfs with the number of tiles with points*)
  dfs map_array numcols starttile
    (Array.init (Array.length map_array) (fun x -> 0))
  + 1
  = num_tiles

(**[draw_grid_y x numcols currentbox] draws the gridlines of tiles on
   the y axis*)
let rec draw_grid_y x numcols currentbox =
  (*draws grid along 6 axis*)
  let pixsize = x / numcols in
  if currentbox <= x - pixsize then (
    set_color (rgb 255 255 255);
    draw_rect 0 currentbox x x;
    draw_grid_y x numcols (currentbox + pixsize))
  else if currentbox <= x then (
    set_color (rgb 178 73 0);
    fill_rect 0 currentbox x (x - currentbox))
  else ()

(**[draw_grid_x x numcols currentbox] draws the gridlines of tiles on
   the x axis*)
let rec draw_grid_x x numcols currentbox =
  (*draws grid along x axis*)
  let pixsize = x / numcols in
  if currentbox <= x - pixsize then (
    set_color (rgb 255 255 255);
    draw_rect currentbox 0 pixsize x;
    draw_grid_x x numcols (currentbox + pixsize))
  else if currentbox <= x then (
    set_color (rgb 178 73 0);
    fill_rect currentbox 0 (x - currentbox) x)
  else ()

(**[fill_map_x lst x row_num col_num pixsize] is a helper function which
   draws tiles to reflect their position and state (i.e. home tile,
   normal tile, ghost start tile)*)
let rec fill_map_x lst x row_num col_num pixsize =
  (*fill map helper function*)
  match lst with
  | [] -> ()
  | h :: t ->
      if h = "1" then (
        set_color (rgb 255 255 255);
        fill_rect (row_num * pixsize) (col_num * pixsize) pixsize
          pixsize;
        fill_map_x t x (row_num + 1) col_num pixsize)
      else if h = "2" then (
        set_color (rgb 0 0 255);
        fill_rect (row_num * pixsize) (col_num * pixsize) pixsize
          pixsize;
        fill_map_x t x (row_num + 1) col_num pixsize)
      else if h = "3" then (
        set_color (rgb 255 0 0);
        fill_rect (row_num * pixsize) (col_num * pixsize) pixsize
          pixsize;
        fill_map_x t x (row_num + 1) col_num pixsize)
      else fill_map_x t x (row_num + 1) col_num pixsize

(**[fill_map selection_list x col_num pixsize] draws visually given the
   list of all the tiles and what states they have*)
let rec fill_map selection_list x col_num pixsize =
  (*draws map given selection*)
  match selection_list with
  | [] -> ()
  | h :: t ->
      fill_map_x (Array.to_list h) x 0 col_num pixsize;
      fill_map t x (col_num - 1) pixsize

(**[map_user selection numcols name draw_state h_times g_times] controls
   the user interface for map building, ensuring both save conditions
   (valid map and having home tile/ghost tile), and handles user modes .
   erase, draw, etc*)
let map_user selection numcols name draw_state h_times g_times =
  if Graphics.key_pressed () then
    let letter = (Graphics.wait_next_event [ Key_pressed ]).key in
    (*letters change mode that it is in*)
    if 'd' = letter then ("1", h_times, g_times)
    else if 'e' = letter then ("0", h_times, g_times)
    else if 'g' = letter then ("3", h_times, g_times + 1)
    else if 'h' = letter then ("2", h_times + 1, g_times)
    else if 'q' = letter then exit 0
    else if 's' = letter then
      (*need at least 1 home tile to exit.... later i might make it that
        you also need all tiles to be accessible*)
      if
        h_times > 0 && g_times > 0
        && check_map
             (Array.to_list
                (Array.map (fun x -> Array.to_list x) selection))
             numcols
      then (
        Csv.save
          ("data/Maps/" ^ name ^ ".csv")
          (Array.to_list
             (Array.map (fun x -> Array.to_list x) selection));
        exit 0)
      else (draw_state, h_times, g_times)
    else (draw_state, h_times, g_times)
  else (draw_state, h_times, g_times)

(**[build_map_draw selection x numcols] draws the map (using commands
   established above)*)
let build_map_draw selection x numcols =
  auto_synchronize false;
  set_color (rgb 0 0 0);
  moveto (x - 590) (x + 5);
  fill_rect 0 0 x x;
  set_color (rgb 50 50 50);
  draw_string
    "Modes: 'e' - erase, 'd' - draw, 's' - save, 'h' - home tile";

  auto_synchronize false;
  set_color (rgb 255 255 255);

  fill_map (Array.to_list selection) x (numcols - 1) (x / numcols);
  draw_grid_x x numcols 0;
  draw_grid_y x numcols 0;
  auto_synchronize true

(**[make_map x name selection numcols draw_state h_times g_times] brings
   together all above functions, handling mouse positions, , calling
   main drawing functions, and key press management*)
let rec make_map x name selection numcols draw_state h_times g_times =
  (*body of this function*)
  let new_draw_state, new_h, new_g =
    map_user selection numcols name draw_state h_times g_times
  in
  build_map_draw selection x numcols;

  let mouse = Graphics.wait_next_event [ Graphics.Button_down; Poll ] in
  if mouse.button then
    let newa = mouse.mouse_x * numcols / x in
    let newb = mouse.mouse_y * numcols / x in
    (*where we actually change the data type*)
    let _ =
      try
        Array.set
          (Array.get selection (numcols - 1 - newb))
          newa draw_state
      with
      | Invalid_argument _ -> ()
    in
    make_map x name selection numcols new_draw_state new_h new_g
  else make_map x name selection numcols new_draw_state new_h new_g

(**[write_name name x] is a fucntion which enables a user to type their
   name*)
let rec write_name name x =
  let press1 = wait_next_event [ Key_pressed ] in
  if press1.key = '.' then name
  else if press1.key = '\b' then (
    (*be able to handle backspace since users seem to try that alot and
      it causes issues*)
    moveto ((size_x () / 2) - x) ((size_y () / 2) - 300);
    set_color (rgb 194 178 128);
    fill_rect ((size_x () / 2) - x - 10) ((size_y () / 2) - 300) 10 15;
    set_color (rgb 255 255 255);
    if String.length name = 0 then write_name name x
    else write_name (String.sub name 0 (String.length name - 1)) (x + 10))
  else (
    moveto ((size_x () / 2) - x) ((size_y () / 2) - 300);
    draw_string (Char.escaped press1.key);
    write_name (name ^ Char.escaped press1.key) (x - 10))

let main x y =
  set_color (rgb 194 178 128);
  fill_rect 0 0 x y;
  moveto ((size_x () / 2) - 200) ((size_y () / 2) - 200);
  set_color (rgb 255 255 255);
  draw_string "type a 2 digit number (<30) to initalize map size";
  moveto ((size_x () / 2) - 55) ((size_y () / 2) + 200);
  draw_string "Map Builder!";
  let press1 = wait_next_event [ Key_pressed ] in
  let press2 = wait_next_event [ Key_pressed ] in
  let numcols =
    ((Char.code press1.key - 48) * 10) + (Char.code press2.key - 48)
  in
  moveto ((size_x () / 2) - 150) ((size_y () / 2) - 250);
  set_color (rgb 255 255 255);
  draw_string "type map name followed by a period";
  make_map x (write_name "" 150)
    (Array.init numcols (fun x -> Array.init numcols (fun x -> "0")))
    numcols "1" 0 0
