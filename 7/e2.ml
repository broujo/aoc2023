open Helpers
open Parser

let test_input = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

type hand_type = High | Pair | TwoPair | Three | Full | Four | Five

let card_value c =
  match c with
  | C 'T' -> 10
  | C 'J' -> 1
  | C 'Q' -> 12
  | C 'K' -> 13
  | C 'A' -> 14
  | C cc -> CCChar.to_int cc - CCChar.to_int '0' 

let pp_card fmt c =
  match c with
  | C cc -> CCFormat.fprintf fmt "%c" cc

let compare_card c d =
  CCInt.compare (card_value c) (card_value d)

let find_type h =
  (* CCFormat.printf "%a@." (CCList.pp pp_card) h; *)
  let eq = (=) in
  let f v = match v with None -> Some 1 | Some h -> Some (h + 1) in
  CCList.fold_right (CCList.Assoc.update ~eq ~f) h []
  |> CCList.sort (fun (_, b) (_, a) -> CCInt.compare a b)
  |> fun assoc ->
      let j_val = CCOption.get_or ~default:0 (CCList.Assoc.get ~eq (C 'J') assoc) in 
      match assoc with
      | [] -> failwith "impossible"
      | (_, 5) :: [] -> Five
      | (_, 4) :: _ when j_val = 1 -> Five
      | (C 'J', 4) :: [_] -> Five
      | (_, 3) :: _ when j_val = 2 -> Five
      | (C 'J', 3) :: (_, 2) :: _ -> Five

      | (_, 4) :: [_] -> Four
      | (_, 3) :: _ when j_val = 1 -> Four
      | (_, 2) :: (C 'J', 2) :: _ -> Four
      | (C 'J', 2) :: (_, 2) :: _ -> Four
      | (C 'J', 3) :: _ -> Four

      | (_, 3) :: (_, 2) :: [] -> Full
      | (_, 2) :: (_, 2) :: _ when j_val = 1 -> Full

      | (_, 3) :: [ _; _ ] -> Three
      | (_, 2) :: _ when j_val = 1 -> Three
      | (C 'J', 2) :: _ -> Three

      | (_, 2) :: (_, 2) :: [ _ ] -> TwoPair
      | (_, 2) :: _ when j_val = 1 -> TwoPair

      | (_, 2) :: _ -> Pair
      | (_, 1) :: _ when j_val = 1 -> Pair

      | (_, 1) :: _ -> High
      | _ -> failwith "impossible too"

let hand_compare h i =
  let th = find_type h in
  let ti = find_type i in
  if th <> ti
  then compare th ti
  else
    let rec aux h i =
      match h, i with
      | [], [] -> failwith "not sure how to handle equality"
      | th :: qh, ti :: qi when th = ti -> aux qh qi
      | th :: _, ti :: _ -> compare_card th ti
      | _, _ -> failwith "not same size"
    in aux h i

let winnings hands =
  CCList.sort (fun (h,_) (i,_) -> hand_compare h i) hands 
  |> CCList.mapi (fun i (_, bid) -> (i + 1) * bid)

let run input =
  CCString.lines input
  |> CCList.map parse
  |> winnings
  |> CCList.fold_left (+) 0

let () =
  assert (run test_input == 5905);

  let input = Lwt_main.run (Aoc.input 7) in
  print_int (run input)
