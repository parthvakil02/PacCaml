open State

exception Empty

(*use for broken chasing paccaml stuff: go (match Ghosts.chase_paccaml
  st st.ghosts.python.tile with | [] -> raise Empty | h :: t -> h) st
  "pythongeist";*)
type const = {
  x : int;
  y : int;
  starttile : int;
  startghosttile : int;
  maxpoints : int;
  speed : float;
  mapname : string;
  levelnum : int;
}

let update_state
    map_tuple
    next_tile
    bonus_eat
    st
    ghosttile
    laststate
    power_eat
    levelnum
    power =
  {
    map = snd map_tuple;
    pacState = next_tile;
    ghosts =
      {
        java =
          (if power = false || identify_collision st laststate <> "java"
          then go (Ghosts.random_ghost st "javarition") st "javarition"
          else { tile = ghosttile; orientation = Up });
        cpirit =
          (if
           power = false || identify_collision st laststate <> "cpirit"
          then go (Ghosts.random_ghost st "cpirit") st "cpirit"
          else { tile = ghosttile; orientation = Up });
        python =
          (if
           power = false || identify_collision st laststate <> "python"
          then
           go (Ghosts.random_ghost st "pythongeist") st "pythongeist"
          else { tile = ghosttile; orientation = Up });
        sqhell =
          (if
           power = false || identify_collision st laststate <> "sqhell"
          then go (Ghosts.random_ghost st "sqhell") st "sqhell"
          else { tile = ghosttile; orientation = Up });
      };
    points =
      (if
       fst map_tuple
       && Levels.get_id next_tile.tile != Levels.get_id st.pacState.tile
      then st.points + 10
      else st.points);
    lives = st.lives;
    bonuspoints =
      ((if bonus_eat then st.bonuspoints + (100 * levelnum)
       else st.bonuspoints)
      + (if power then 200 else 0)
      + if power_eat then 50 else 0);
  }

let init_state map points lives starttile startghosttile =
  {
    map;
    pacState =
      { tile = Levels.get_tile map starttile; orientation = Right };
    ghosts =
      {
        java =
          {
            tile = Levels.get_tile map startghosttile;
            orientation = Right;
          };
        python =
          {
            tile = Levels.get_tile map startghosttile;
            orientation = Up;
          };
        cpirit =
          {
            tile = Levels.get_tile map startghosttile;
            orientation = Left;
          };
        sqhell =
          {
            tile = Levels.get_tile map startghosttile;
            orientation = Down;
          };
      };
    points;
    lives;
    bonuspoints = 0;
  }

let die_state st starttile startghosttile =
  {
    map = st.map;
    pacState =
      { tile = Levels.get_tile st.map starttile; orientation = Right };
    ghosts =
      {
        java =
          {
            tile = Levels.get_tile st.map startghosttile;
            orientation = Right;
          };
        python =
          {
            tile = Levels.get_tile st.map startghosttile;
            orientation = Up;
          };
        cpirit =
          {
            tile = Levels.get_tile st.map startghosttile;
            orientation = Left;
          };
        sqhell =
          {
            tile = Levels.get_tile st.map startghosttile;
            orientation = Down;
          };
      };
    points = st.points;
    lives = st.lives - 1;
    bonuspoints = st.bonuspoints;
  }

(*fixes spamming issue by never letting a queue pile up*)

let rec resolve_queue dir =
  (*the way the graphics keypressed function works sometimes leads to
    commands being queued up. THis function (called in move) just drains
    that queue so it never builds up and becomes an issue*)
  if Graphics.key_pressed () then
    let new_dir = Graphics.wait_next_event [ Key_pressed ] in
    resolve_queue new_dir
  else dir

let move_queue st queue =
  if Graphics.key_pressed () then
    let status = Graphics.wait_next_event [ Graphics.Key_pressed ] in
    let quit = status.key in
    let move_dir = Command.translate_move (resolve_queue status) in

    if move_dir = Command.Quit || quit = 'q' then (
      History.save st.points;
      exit 0)
    else if
      Levels.get_id (go move_dir st "caml").tile
      != Levels.get_id st.pacState.tile
    then (move_dir, move_dir)
    else (move_dir, On st.pacState.orientation)
  else if
    Levels.get_id (go queue st "caml").tile
    != Levels.get_id st.pacState.tile
  then (queue, queue)
  else (queue, On st.pacState.orientation)