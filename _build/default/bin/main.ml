(* open Paccaml *)

(* open Paccaml.Welcome *)
open Paccaml
open Pc_graphics
open Levels
open Graphics
open State
open History
open Gamestate

let stall new_time old_time speed =
  Unix.sleepf (speed -. new_time +. old_time)

let rec update
    st
    queue
    ctr
    laststate
    bonustile
    disp_bon
    powertiles
    power_turn
    const =
  let time = Unix.gettimeofday () in
  Random.init (int_of_float time);
  update_graphics st bonustile powertiles const disp_bon;
  if st.points = const.maxpoints then (st.lives, st.bonuspoints)
  else
    let coll = check_collision st laststate in
    let bonustile, ctr, bonus_eat =
      update_bonus_tile st ctr const.starttile bonustile
    in
    let powertiles, power, eaten_tile = eat_power_tile powertiles st in
    let power_turn =
      if power then 35
      else if power_turn = 0 then power_turn
      else power_turn - 1
    in
    let disp_bon =
      disp_bonus_maintain disp_bon power eaten_tile bonus_eat
        const.levelnum const.starttile
        (coll && power_turn > 0)
        (Levels.get_id st.pacState.tile)
    in
    let new_time = Unix.gettimeofday () in
    let new_queue, move = Gamestate.move_queue st queue in
    draw_sprites st const.x move queue ctr (power_turn != 0);
    let ctr = ctr + 1 in
    stall new_time time const.speed;
    if coll && power_turn = 0 then
      if st.lives <> 1 then (
        if death_screen const.x const.y then ()
        else (
          History.save (st.points + st.bonuspoints);
          exit 0);
        Unix.sleepf 0.5;
        update
          (Gamestate.die_state st const.starttile const.startghosttile)
          queue ctr st None disp_bon powertiles power_turn const)
      else (
        you_lose_screen const.x const.y;
        Unix.sleepf 0.5;
        History.save (st.points + st.bonuspoints);
        (0, st.bonuspoints))
    else
      let next_tile = go move st "caml" in
      let map_tuple = eat_map_tile st next_tile.tile in
      update
        (Gamestate.update_state map_tuple next_tile bonus_eat st
           (Levels.get_tile (snd map_tuple) const.startghosttile)
           laststate power const.levelnum coll)
        new_queue ctr st bonustile disp_bon powertiles power_turn const

let rec play x y speed points lives levels =
  let files = Sys.readdir "data/Maps" in
  let mapname = Array.get files ((levels + 1) mod Array.length files) in
  let file_name = "data/Maps/" ^ mapname in
  let map, starttile, startghosttile, numpoints = from_csv file_name in
  let newx = Levels.get_numcols map * 40 in
  let newy = (Levels.get_numcols map * 40) + 50 in
  Graphics.resize_window newx newy;
  let state =
    Gamestate.init_state map points lives starttile startghosttile
  in
  let const =
    {
      x = newx;
      y = newy;
      speed;
      maxpoints = state.points + numpoints;
      starttile;
      startghosttile;
      mapname;
      levelnum = levels;
    }
  in
  let next_lives, bonuspoints =
    update
      {
        state with
        map = snd (eat_map_tile state (get_tile map starttile));
      }
      Command.Off points state None [] (make_power_tiles map) 0 const
  in
  if next_lives > 0 then
    if update_screen newx newy then
      play newx newy
        (if const.levelnum mod 3 = 0 then speed -. 0.01 else speed)
        (points + numpoints + bonuspoints)
        next_lives (levels + 1)
    else save (state.points + numpoints + bonuspoints)
  else ()

let rec main () =
  let points = init () in
  let x = 600 in
  let y = 650 in
  Random.init (int_of_float (Unix.time ()));
  try
    welcome_screen points x y;
    play x y 0.2 0 3 1;
    resize_window x y;
    main ()
  with
  | Graphics.Graphic_failure _ -> ()

let () = main ()