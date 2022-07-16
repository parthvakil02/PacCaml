(*for maintaining player statistics, specfici to person computer. one
  may ask why we have this feature.... good question*)

let points_grabber = function
  (*takes first value from that csv*)
  | [] -> (0, 0)
  | h :: h2 :: t -> (int_of_string h, int_of_string h2)
  | h :: t -> (int_of_string h, 0)

let init () =
  let remember = Sys.readdir "data/" in
  if Array.mem "history.csv" remember then
    let csv = Csv.load "data/history.csv" in
    match csv with
    | [] -> (0, 0)
    | h :: t -> points_grabber h
  else (
    (*make csv with points = 0*)
    Csv.save "data/history.csv" [ [ "0"; "0" ] ];
    0,0)

let save points =
  let csv = Csv.load "data/history.csv" in

  let new_points, highscore =
    match csv with
    | [] -> (0, 0)
    | h::t-> (fst (points_grabber h) + points, snd (points_grabber h))
  in
  Csv.save "data/history.csv"
    [
      [
        string_of_int new_points;
        (if points > highscore then string_of_int points
        else string_of_int highscore);
      ];
    ]
