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
let square x = x *. x
let delta a b c = square b -. (4. *. a *. c)

let solve a b c =
  if a = 0. then failwith "Not a quadratic equation\n"
  else if delta a b c >= 0. then
    let x1 = (-.b +. sqrt (delta a b c)) /. (2. *. a)
    and x2 = (-.b -. sqrt (delta a b c)) /. (2. *. a) in
    (x1, x2)
  else failwith "not real solutions"

let concat l =
  CCList.map string_of_int l |> CCList.fold_left (^) "" |> int_of_string

let find_sol t d =
  let x1, x2 = solve 1. (-.(float_of_int t)) (float_of_int d) in
  let mi, ma =
    if x1 > x2 then
      x2, x1 else x1,x2 in
  let mi = Float.to_int (Float.ceil mi) in
  let ma = Float.to_int (Float.floor ma) in
  ma - mi + 1
    

let run input =
  let races = parse input in
  let time, distance = CCList.split races in
  let time = concat time in
  let distance = concat distance in
  find_sol time distance
  

let () =
  assert (run test_input == 71503);

  let input = Lwt_main.run (Aoc.input 6) in
  print_int (run input)
