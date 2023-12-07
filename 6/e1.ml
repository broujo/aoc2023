open Helpers
open Parser

let test_input = "Time:      7  15   30\nDistance:  9  40  200"

let find_min (t, d) =
  let rec aux tp = if tp * (t - tp) > d then tp else aux (tp + 1) in
  aux 1

let find_max (t, d) =
  let rec aux tp = if tp * (t - tp) > d then tp else aux (tp - 1) in
  aux (t - 1)

let do_nothing _a _b = ()

let run input =
  let races = parse input in
  let mins = CCList.map find_min races in
  let maxs = CCList.map find_max races in
  let poss = CCList.map2 (fun a b -> b - a + 1) mins maxs in
  CCList.fold_right ( * ) poss 1

let () =
  assert (run test_input == 288);

  let input = Lwt_main.run (Aoc.input 6) in
  print_int (run input)
