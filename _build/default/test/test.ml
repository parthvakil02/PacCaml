(*PacCaml Testing Philosophy:

  our testing philosophy, as a visually structure game, oriented itself
  around ensuring reliable base functionality and thorough playtesting
  to ensure functionality between the modules. There was a lot of
  playtesting. Between all of us, we had a million points of paccaml.
  The average points per game in paccaml is around 1-5 thousand, so
  obviously we have played a lot.button

  Early in the process, when designing the infrastructure of paccaml,
  testing was essential in ensuring the correctness of our
  infrastructure, in handling edge cases correctly, and even in testing
  some of the UI features.

  Since then, we have placed a large emphasis on playtesting as that is
  what allows us to see the interactions between these different
  modules.

  If a system is built of correct pieces, and the pieces fit together in
  orthoganal ways (which we tried to maintain), than this ensures that
  the system itself is correct.*)

open OUnit2
open Paccaml
open Levels
open State
open Command
open Graphics

let sample_map, _, _, _ = from_csv "data/TestMaps/map1.csv"

let sample_tile = get_tile sample_map 1

let sample_tile_other = get_tile sample_map 5

let sample_tile_next = get_tile sample_map 21

let sample_tile_next_right = get_tile sample_map 6

let sample_state =
  {
    map = sample_map;
    pacState = { tile = sample_tile; orientation = Down };
    ghosts =
      {
        java = { tile = sample_tile_other; orientation = Right };
        python = { tile = sample_tile; orientation = Right };
        cpirit = { tile = sample_tile; orientation = Right };
        sqhell = { tile = sample_tile; orientation = Right };
      };
    points = 0;
    lives = 3;
    bonuspoints = 0;
  }

let state_tests =
  [
    (*paccaml tests*)
    ( "current tile is 0 paccaml" >:: fun _ ->
      assert_equal (current_tile sample_state "caml") sample_tile );
    ( "current direction is Down paccamnl" >:: fun _ ->
      assert_equal (current_direction sample_state "caml") Down );
    ( "up is valid move" >:: fun _ ->
      assert_equal
        (go (On Up) sample_state "caml")
        { tile = sample_tile_next; orientation = Up } );
    ( "left is not valid move" >:: fun _ ->
      assert_equal
        (go (On Left) sample_state "caml")
        { tile = sample_tile; orientation = Left } );
    (*java tests*)
    ( "current tile is 5" >:: fun _ ->
      assert_equal
        (current_tile sample_state "javarition")
        sample_tile_other );
    ( "current direction is Down paccamnl" >:: fun _ ->
      assert_equal (current_direction sample_state "javarition") Right
    );
    ( "Right is valid move" >:: fun _ ->
      assert_equal
        (go (On Right) sample_state "javarition")
        { tile = sample_tile_next_right; orientation = Right } );
    ( "left is not valid move" >:: fun _ ->
      assert_equal
        (go (On Left) sample_state "javarition")
        { tile = sample_tile_other; orientation = Left } );
    (*python tests*)
    ( "current tile is 0" >:: fun _ ->
      assert_equal (current_tile sample_state "pythongeist") sample_tile
    );
    ( "up is valid move" >:: fun _ ->
      assert_equal
        (go (On Up) sample_state "pythongeist")
        { tile = sample_tile_next; orientation = Up } );
    ( "left is not valid move" >:: fun _ ->
      assert_equal
        (go (On Left) sample_state "pythongeist")
        { tile = sample_tile; orientation = Left } );
    (*test go for off*)
    ( "off returns same state" >:: fun _ ->
      assert_equal
        (go Off sample_state "pythongeist")
        { tile = sample_tile; orientation = Right } );
    (*test eat_map for bool*)
    ( "test eat_map returns true when eat" >:: fun _ ->
      assert_equal
        (fst (eat_map_tile sample_state (get_tile sample_map 5)))
        true );
    ( "test eat_map returns false when no eat" >:: fun _ ->
      assert_equal
        (fst (eat_map_tile sample_state (get_tile sample_map 4)))
        false );
    (*test check collisions*)
    ( "test check collision" >:: fun _ ->
      assert_equal (check_collision sample_state sample_state) true );
    ( "test check collision on different sample state" >:: fun _ ->
      assert_equal
        (check_collision
           {
             sample_state with
             ghosts =
               {
                 python = sample_state.ghosts.java;
                 java = sample_state.ghosts.java;
                 sqhell = sample_state.ghosts.java;
                 cpirit = sample_state.ghosts.java;
               };
           }
           sample_state)
        false );
    ( "test chek collision final" >:: fun _ ->
      assert_equal
        (check_collision
           {
             sample_state with
             ghosts =
               {
                 python = sample_state.ghosts.java;
                 java = sample_state.ghosts.python;
                 sqhell = sample_state.ghosts.sqhell;
                 cpirit = sample_state.ghosts.cpirit;
               };
           }
           sample_state)
        true );
  ]

let command_tests =
  [
    ( "a is left" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = true;
             key = 'a';
           })
        (On Left) );
    ( "w is up" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = true;
             key = 'w';
           })
        (On Up) );
    ( "s is down" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = true;
             key = 's';
           })
        (On Down) );
    ( "d is right" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = true;
             key = 'd';
           })
        (On Right) );
    ( "if no key is pressed then no move is made" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = false;
             key = 'a';
           })
        Off );
    ( "if random not key is pressed then no move is made" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = true;
             key = 'b';
           })
        Off );
    ( "if no q is pressed then quit" >:: fun _ ->
      assert_equal
        (translate_move
           {
             mouse_x = 0;
             mouse_y = 0;
             button = false;
             keypressed = true;
             key = 'q';
           })
        Quit );
  ]

let test_map1, _, _, test_count_points =
  from_csv "data/TestMaps/map1.csv"

let test_map2, _, _, _ = from_csv "data/TestMaps/map2.csv"

let test_tile1 = get_tile test_map1 0

let test_pass_tile = get_tile test_map1 1

let test_next_tile name map tile direction exp =
  name >:: fun _ ->
  assert_equal
    (get_id (next_tile map (get_tile map tile) direction))
    exp

let test_pacmap, pac_start_tile, ghost_start_tile, numpoints =
  from_csv "data/Maps/PacMap.csv"

let levels_tests =
  [
    (*test tile 1 *)
    ( "test get_id/get_tile" >:: fun _ ->
      assert_equal (get_id test_tile1) 0 );
    ( "test passability/get_tile" >:: fun _ ->
      assert_equal (passibility test_tile1) false );
    ( "test\n      food/get_tile" >:: fun _ ->
      assert_equal (food test_tile1) 0 );
    (*test passible tile*)
    ( "test get_id/get_tile" >:: fun _ ->
      assert_equal (get_id test_pass_tile) 1 );
    ( "test passability/get_tile" >:: fun _ ->
      assert_equal (passibility test_pass_tile) true );
    ( "test food/get_tile" >:: fun _ ->
      assert_equal (food test_pass_tile) 1 );
    (*test next_tile*)
    test_next_tile "test North" test_map1 5 Up 25;
    test_next_tile "test SOuth" test_map1 25 Down 5;
    test_next_tile "test East" test_map1 5 Right 6;
    test_next_tile "test West" test_map1 28 Left 27;
    test_next_tile "test South fail impassible" test_map1 4 Up 4;
    test_next_tile "test North fail impassible" test_map1 24 Down 24;
    test_next_tile "test East fail impassible" test_map1 3 Right 3;
    test_next_tile "test West fail impassible" test_map1 3 Left 3;
    test_next_tile "test South Wrap" test_map2 5 Down 425;
    test_next_tile "test North warp" test_map2 425 Up 5;
    test_next_tile "test East Wrap" test_map2 20 Right 0;
    test_next_tile "test West weap" test_map2 0 Left 20;
    ( "test num_cols" >:: fun _ ->
      assert_equal (get_numcols test_map1) 20 );
    ("test numpoints" >:: fun _ -> assert_equal test_count_points 2980);
    (*test pacmap stuff*)
    ("test numpoints pacmap" >:: fun _ -> assert_equal numpoints 2020);
    ("test start_tile" >:: fun _ -> assert_equal pac_start_tile 115);
    ( "test ghosts start_tile" >:: fun _ ->
      assert_equal ghost_start_tile 241 );
  ]

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let pp_tile (tile : Levels.tile) =
  "\"" ^ string_of_int (Levels.get_id tile) ^ "\""

let rec get_tile_list pac st lst =
  match lst with
  | [] -> []
  | h :: t -> (go h st pac).tile :: get_tile_list pac st t

let test_find_route
    (name : string)
    (pac : string)
    (st : State.t)
    (start : Levels.tile)
    (final : Levels.tile)
    (visited : bool array)
    (stack : Command.command list)
    (expected_output : Levels.tile list) =
  name >:: fun _ ->
  assert_equal expected_output
    (get_tile_list pac st
       (Ghosts.find_route st.map start final visited stack))
    ~printer:(pp_list pp_tile)

let init_visited map =
  let height = Levels.get_numcols map in
  Array.init (height * height) (fun x -> false)

let find_route_tests =
  [
    test_find_route "Same start and end tile returns []" "pythongeist"
      sample_state sample_state.ghosts.python.tile
      sample_state.ghosts.python.tile
      (init_visited sample_state.map)
      [] [];
    test_find_route "Move up one tile" "pythongeist" sample_state
      sample_state.ghosts.python.tile
      (get_tile sample_map 21)
      (init_visited sample_state.map)
      []
      [ get_tile sample_map 21 ];
    test_find_route "Move left one tile" "pythongeist"
      {
        sample_state with
        ghosts =
          {
            sample_state.ghosts with
            python =
              {
                sample_state.ghosts.python with
                tile = get_tile sample_map 22;
              };
          };
      }
      (get_tile sample_map 22)
      (get_tile sample_map 21)
      (init_visited sample_state.map)
      []
      [ get_tile sample_map 21 ];
    test_find_route "Move right one tile" "pythongeist"
      {
        sample_state with
        ghosts =
          {
            sample_state.ghosts with
            python =
              {
                sample_state.ghosts.python with
                tile = get_tile sample_map 21;
              };
          };
      }
      (get_tile sample_map 21)
      (get_tile sample_map 22)
      (init_visited sample_state.map)
      []
      [ get_tile sample_map 22 ];
    test_find_route "Move down one tile" "pythongeist"
      {
        sample_state with
        ghosts =
          {
            sample_state.ghosts with
            python =
              {
                sample_state.ghosts.python with
                tile = get_tile sample_map 21;
              };
          };
      }
      (get_tile sample_map 21)
      (get_tile sample_map 1)
      (init_visited sample_state.map)
      []
      [ get_tile sample_map 1 ];
  ]

let ghosts_tests = List.flatten [ find_route_tests ]

let game_state_tests = List.flatten []

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           command_tests;
           state_tests;
           levels_tests;
           ghosts_tests;
           game_state_tests;
         ]

let _ = run_test_tt_main suite