open Graphics
open Csv

let pick_caml_colors mode = function
  | "-" -> black
  | "a" -> white
  | "b" ->
      if mode then Graphics.rgb 230 76 0 else Graphics.rgb 255 121 0
  | "c" ->
      if mode then Graphics.rgb 255 136 77 else Graphics.rgb 196 98 16
  | _ ->
      if mode then Graphics.rgb 255 166 77 else Graphics.rgb 255 215 0

let pick_python_colors mode = function
  | "c" -> black
  | "-" -> white
  | "b" -> Graphics.rgb 54 124 194
  | "d" ->
      if mode then Graphics.rgb 54 124 194 else Graphics.rgb 255 230 0
  | "e" -> Graphics.rgb 161 208 255
  | "f" ->
      if mode then Graphics.rgb 161 208 255 else Graphics.rgb 199 171 72
  | _ -> Graphics.rgb 214 32 32

let pick_java_colors mode = function
  | "-" -> black
  | "b" -> if mode then Graphics.rgb 54 124 194 else white
  | "c" -> Graphics.rgb 123 179 242
  | "d" ->
      if mode then Graphics.rgb 0 64 208 else Graphics.rgb 201 201 201
  | _ -> if mode then white else Graphics.rgb 145 88 47

let pick_cpirit_colors mode = function
  | "-" -> black
  | "b" -> if mode then Graphics.rgb 123 179 242 else white
  | "c" ->
      if mode then Graphics.rgb 54 124 194 else Graphics.rgb 255 199 251
  | _ -> Graphics.rgb 214 214 214

let pick_sqhell_colors mode = function
  | "-" -> black
  | "b" ->
      if mode then Graphics.rgb 123 179 242
      else Graphics.rgb 212 175 144
  | "c" ->
      if mode then Graphics.rgb 54 124 194 else Graphics.rgb 161 104 58
  | "d" -> if mode then Graphics.rgb 0 68 204 else Graphics.rgb 99 55 20
  | _ -> Graphics.rgb 194 52 52

let pick_cactus_colors = function
  | "-" -> Graphics.rgb 124 196 133
  | "b" -> black
  | "c" -> Graphics.rgb 135 108 70
  | "d" -> Graphics.rgb 255 186 252
  | "e" -> Graphics.rgb 68 145 78
  | _ -> Graphics.rgb 239 242 72

let pick_leaves_colors = function
  | "-" -> black
  | "b" -> Graphics.rgb 124 196 133
  | "c" -> Graphics.rgb 178 255 188
  | _ -> Graphics.rgb 57 148 70

(*Convert csv char list list to color array list... will need to add
  more colors later*)
let rec find_color img f =
  match img with
  | [] -> []
  | h :: t -> Array.of_list (List.map f h) :: find_color t f

let build_pac tile numcols x img schema mode =
  (*two notes on this function. 2. Images are loaded from csv file img*)
  let funct =
    match schema with
    | "javarition" -> pick_java_colors mode
    | "pythongeist" -> pick_python_colors mode
    | "cpirit" -> pick_cpirit_colors mode
    | "sqhell" -> pick_sqhell_colors mode
    | "cactus" -> pick_cactus_colors
    | "leaves" -> pick_leaves_colors
    | _ -> pick_caml_colors mode
  in
  let pix_size = x / numcols in
  let id = Levels.get_id tile in
  draw_image
    (make_image (Array.of_list (find_color (load img) funct)))
    (id mod numcols * pix_size)
    (id / numcols * pix_size)

let build_pac_onloc locx locy img schema mode =
  (*Images are loaded from csv file img*)
  let funct =
    match schema with
    | "javarition" -> pick_java_colors mode
    | "pythongeist" -> pick_python_colors mode
    | "cpirit" -> pick_cpirit_colors mode
    | "sqhell" -> pick_sqhell_colors mode
    | "cactus" -> pick_cactus_colors
    | "leaves" -> pick_leaves_colors
    | _ -> pick_caml_colors mode
  in
  draw_image
    (make_image (Array.of_list (find_color (load img) funct)))
    locx locy

let disp_bon_drawer id points pix_size numcols =
  (*for each part of the list of bonus displays, draws the map*)
  moveto
    ((id mod numcols * pix_size)
    + if points < 100 then pix_size / 4 else 0)
    ((id / numcols * pix_size) + (pix_size / 4));
  draw_string (string_of_int points)

let rec draw_disp_bon disp_bon numcols x =
  (*recursively draws the little point things that show up when you eat
    something worth more than a dot. *)
  set_color white;
  let pix_size = x / numcols in
  match disp_bon with
  | [] -> ()
  | (loc, _, points) :: t ->
      disp_bon_drawer loc points pix_size numcols;
      draw_disp_bon t numcols x

let write_points y margin =
  (*writes number of points on the screen*)
  set_color (rgb 0 0 0);
  moveto 60 (y - margin + 65);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string "Points:";
  draw_rect (margin - 10)
    (y - margin + 55)
    (y - (2 * margin) + 10)
    (margin - 20)

let write_map y margin map =
  (*writes which map is playing at the top of the scrteen*)
  set_color (rgb 0 0 0);
  moveto 190 (y - margin + 65);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string "| Map: ";
  draw_string ("\"" ^ List.hd (String.split_on_char '.' map) ^ "\"")

let write_map_num y margin round =
  (*writes what the level you are on is*)
  set_color (rgb 0 0 0);
  moveto (y - 250) (y - margin + 65);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string "| Level: ";
  draw_string (string_of_int round)

let update_points points y margin =
  (*makes rectangle at top of screen, displays number of points on top
    of that*)
  set_color (Graphics.rgb 245 245 220);
  fill_rect 0 y y 50;
  set_color (rgb 0 0 0);
  moveto 140 (y - margin + 65);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string (Int.to_string points)

let update_lives lives y margin =
  (*displays number of lives on the screen*)
  moveto (y - (3 * margin)) (y - margin + 65);
  draw_string "| Lives:";
  set_color (rgb 0 0 0);
  moveto (y - 65) (y - margin + 65);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string (Int.to_string lives)

let introductions x =
  (*writes the "introductions" to the characters on the welcome screen -
    just lots of text,, not data or dat strctures are handled here*)
  draw_string "this is you, PacCaml! ---->";
  build_pac_onloc
    ((size_x () / 2) + 100)
    ((size_y () / 2) + 190)
    "data/Camls/Left1.csv" "pac" false;
  set_color (rgb 255 255 255);
  moveto ((size_x () / 2) - 160) ((size_y () / 2) + 150);
  draw_string "These are your enemies!";
  moveto ((size_x () / 2) - 160) ((size_y () / 2) + 100);
  draw_string "The Undead Javarition! ---->";
  build_pac_onloc
    ((size_x () / 2) + 100)
    ((size_y () / 2) + 90)
    "data/Ghosts/javarition.csv" "javarition" false;
  moveto ((size_x () / 2) - 160) ((size_y () / 2) + 50);
  draw_string "The Ghostly Cpirit! ---->";
  build_pac_onloc
    ((size_x () / 2) + 100)
    ((size_y () / 2) + 40)
    "data/Ghosts/cpirit.csv" "cpirit" false;
  moveto ((size_x () / 2) - 160) ((size_y () / 2) + 0);
  draw_string "S...Q...HELL! ---->";
  build_pac_onloc
    ((size_x () / 2) + 100)
    ((size_y () / 2) - 10)
    "data/Ghosts/sqhell.csv" "sqhell" false;
  moveto ((size_x () / 2) - 160) ((size_y () / 2) - 50);
  draw_string "And The Evil Pythongeist! ->";
  build_pac_onloc
    ((size_x () / 2) + 100)
    ((size_y () / 2) - 60)
    "data/Ghosts/pythongeist.csv" "pythongeist" false;
  build_pac_onloc
    ((size_x () / 2) - 220)
    ((size_y () / 2) - 160)
    "data/Food/cactus.csv" "cactus" false;
  set_color black;
  moveto ((size_x () / 2) - 230) ((size_y () / 2) - 180);
  draw_string "Cactus";
  moveto ((size_x () / 2) - 250) ((size_y () / 2) - 200);
  draw_string "Give Powers!";
  build_pac_onloc
    ((size_x () / 2) + 190)
    ((size_y () / 2) - 160)
    "data/Food/leaves.csv" "leaves" false;
  moveto ((size_x () / 2) + 180) ((size_y () / 2) - 180);
  draw_string "Leaves";
  moveto ((size_x () / 2) + 150) ((size_y () / 2) - 200);
  draw_string "Give Points!"

let rec welcome_screen life_points x y =
  auto_synchronize false;
  open_graph (" " ^ string_of_int x ^ "x" ^ string_of_int y);
  set_color (rgb 194 178 128);
  fill_rect 0 0 x y;

  moveto ((size_x () / 2) - 110) (size_y () - 50);
  set_font "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1";
  set_color (rgb 0 0 0);
  draw_string "Welcome to PacCaml!";
  moveto ((size_x () / 2) - 92) ((size_y () / 2) - 100);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string "Press 'X' to continue";
  moveto ((size_x () / 2) - 92) ((size_y () / 2) - 150);
  draw_string "use W,A,S,D controls";
  set_color (rgb 255 255 255);
  moveto ((size_x () / 2) - 100) ((size_y () / 2) - 200);
  draw_string "or press p to build a map";
  set_color (rgb 0 0 0);
  moveto
    ((size_x () / 2)
    - 70
    - (5 * String.length (string_of_int (fst life_points))))
    ((size_y () / 2) - 250);
  draw_string ("Lifetime Points: " ^ string_of_int (fst life_points));
  moveto
    ((size_x () / 2)
    - 50
    - (5 * String.length (string_of_int (snd life_points))))
    ((size_y () / 2) - 265);
  draw_string ("Highscore: " ^ string_of_int (snd life_points));
  moveto ((size_x () / 2) - 160) ((size_y () / 2) + 200);
  introductions x;
  auto_synchronize true;
  let x_press = wait_next_event [ Key_pressed; Button_down ] in
  if x_press.keypressed = true then
    if x_press.key = 'x' then fill_rect 0 0 x y
    else if x_press.key = 'q' then exit 0
    else if x_press.key = 'p' then Mapbuilder.main x y
    else welcome_screen life_points x y
  else welcome_screen life_points x y

let rec update_screen x y =
  open_graph (" " ^ string_of_int x ^ "x" ^ string_of_int y);
  set_color (rgb 194 178 128);
  fill_rect 0 0 x y;
  moveto ((size_x () / 2) - 150) ((size_y () / 2) + 100);
  set_font "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1";
  set_color (rgb 0 0 0);
  draw_string "Press 'X' to keep playing";
  moveto ((size_x () / 2) - 132) ((size_y () / 2) - 50);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  draw_string "Press 'q' to quit";
  moveto ((size_x () / 2) - 132) ((size_y () / 2) - 100);
  draw_string "Good Job!";
  set_color (rgb 255 255 255);
  let x_press = wait_next_event [ Key_pressed ] in
  if x_press.keypressed = true then
    if x_press.key = 'x' then (
      fill_rect 0 0 x y;
      true)
    else if x_press.key = 'q' then false
    else update_screen x y
  else update_screen x y

let rec you_lose_screen x y =
  resize_window x y;
  set_color (rgb 194 178 128);
  auto_synchronize false;
  fill_rect 0 0 x y;
  moveto ((size_x () / 2) - 110) ((size_y () / 2) + 100);
  set_font "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1";
  set_color (rgb 0 0 0);
  draw_string "You have lost the game :( ";
  moveto ((size_x () / 2) - 92) ((size_y () / 2) - 50);
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  moveto ((size_x () / 2) - 92) ((size_y () / 2) - 100);
  set_color (rgb 255 255 255);
  auto_synchronize true;
  let x_press = wait_next_event [ Key_pressed ] in
  if x_press.keypressed = true then ()

let rec death_screen x y =
  resize_window x y;
  set_color (rgb 194 178 128);
  auto_synchronize false;
  fill_rect 0 0 x y;
  moveto ((size_x () / 2) - 110) ((size_y () / 2) + 100);
  set_font "-sony-fixed-medium-r-normal--24-170-100-100-c-120-iso8859-1";
  set_color (rgb 0 0 0);
  draw_string "You have lost a life!";
  moveto ((size_x () / 2) - 250) ((size_y () / 2) - 50);
  draw_string "Press a key to continue playing!";
  set_font "-misc-fixed-bold-r-normal--15-140-75-75-c-90-iso8859-1";
  moveto ((size_x () / 2) - 140) ((size_y () / 2) - 100);
  set_color (rgb 255 255 255);
  auto_synchronize true;
  let x_press = wait_next_event [ Key_pressed ] in
  if x_press.key = 'q' then false else true

let rec tile_builder tilelist id pix_size numcols bonustile powertiles x
    =
  set_color
    (if Levels.passibility (Array.get tilelist id) = false then
     Graphics.rgb 245 245 220
    else black);
  fill_rect
    (id mod numcols * pix_size)
    (id / numcols * pix_size)
    ((id mod numcols * pix_size) + pix_size)
    (((id / numcols) + 1) * pix_size);
  set_color green;
  if Levels.food (Array.get tilelist id) = 1 then (
    set_color yellow;
    fill_circle
      ((id mod numcols * pix_size) + (pix_size / 2))
      ((id / numcols * pix_size) + (pix_size / 2))
      2)
  else ();
  if bonustile <> None && Option.get bonustile = id then
    build_pac (Array.get tilelist id) numcols x "data/Food/leaves.csv"
      "leaves" false
  else ();
  if List.mem id powertiles then
    build_pac (Array.get tilelist id) numcols x "data/Food/cactus.csv"
      "cactus" false
  else ();
  if id < Array.length tilelist - 1 then
    tile_builder tilelist (id + 1) pix_size numcols bonustile powertiles
      x
  else ()

(*x and y should be the same for now. having it square is easy*)
let build_map tilelist numcols x bonustile powertiles =
  let pix_size = x / numcols in
  tile_builder tilelist 0 pix_size numcols bonustile powertiles x

let rec pac_image newdir olddir ctr =
  match (newdir, ctr) with
  | Command.On Left, 0 -> "data/Camls/Left1.csv"
  | Command.On Left, _ -> "data/Camls/Left2.csv"
  | Command.On Right, 0 -> "data/Camls/Right1.csv"
  | Command.On Right, _ -> "data/Camls/Right2.csv"
  | Command.On Up, 0 -> "data/Camls/Up1.csv"
  | Command.On Up, _ -> "data/Camls/Up2.csv"
  | Command.On Down, 0 -> "data/Camls/Down1.csv"
  | Command.On Down, _ -> "data/Camls/Down2.csv"
  | Command.Quit, _ -> pac_image olddir olddir ctr
  | Command.Off, x -> pac_image olddir olddir ctr

let rec ghost_image ctr ghost =
  let g_images =
    match ghost with
    | "javarition" ->
        ("data/Ghosts/javarition.csv", "data/Ghosts/javarition2.csv")
    | "pythongeist" ->
        ("data/Ghosts/pythongeist.csv", "data/Ghosts/pythongeist2.csv")
    | "cpirit" -> ("data/Ghosts/cpirit.csv", "data/Ghosts/cpirit2.csv")
    | "sqhell" -> ("data/Ghosts/sqhell.csv", "data/Ghosts/sqhell2.csv")
    | _ -> ("", "")
  in
  match ctr with
  | 0 -> fst g_images
  | _ -> snd g_images

let draw_sprites (st : State.t) x move queue ctr mode =
  auto_synchronize false;
  build_pac st.pacState.tile
    (Levels.get_numcols st.map)
    x
    (pac_image move queue (ctr mod 2))
    "caml" mode;
  build_pac st.ghosts.java.tile
    (Levels.get_numcols st.map)
    x
    (ghost_image (ctr mod 2) "javarition")
    "javarition" mode;
  build_pac st.ghosts.python.tile
    (Levels.get_numcols st.map)
    x
    (ghost_image (ctr mod 2) "pythongeist")
    "pythongeist" mode;
  build_pac st.ghosts.cpirit.tile
    (Levels.get_numcols st.map)
    x
    (ghost_image (ctr mod 2) "cpirit")
    "cpirit" mode;
  build_pac st.ghosts.sqhell.tile
    (Levels.get_numcols st.map)
    x
    (ghost_image (ctr mod 2) "sqhell")
    "sqhell" mode;
  auto_synchronize true

let update_graphics
    (st : State.t)
    bonustile
    powertiles
    (const : Gamestate.const)
    disp_bon =
  auto_synchronize false;
  build_map
    (Levels.get_maplist st.map)
    (Levels.get_numcols st.map)
    const.x bonustile powertiles;
  (*upgrade points has to precede write points for reasons*)
  update_points (st.points + st.bonuspoints) const.x 50;
  write_points const.x 50;
  update_lives st.lives const.x 50;
  write_map const.x 50 const.mapname;
  write_map_num const.x 50 const.levelnum;
  draw_rect 0 0 (const.x - 1) const.x;
  draw_rect 1 1 (const.x - 2) (const.x - 1);
  draw_disp_bon disp_bon (Levels.get_numcols st.map) const.x;
  auto_synchronize true
