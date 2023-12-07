open Helpers
open Parser

let test_input =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

module S = CCSet.Make (CCInt)

let get_score c =
  let card = S.of_list c.card in
  let owned = S.of_list c.owned in
  S.inter card owned |> S.cardinal |> fun i ->
  match i with 0 -> 0 | i -> CCInt.pow 2 (i - 1)

let run input =
  input |> CCString.lines |> CCList.map Parser.parse |> CCList.map get_score
  |> CCList.fold_left ( + ) 0

let () =
  assert (run test_input == 13);

  let input = Lwt_main.run (Aoc.input 4) in
  print_int (run input)
