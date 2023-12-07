open Helpers
open Helpers.Mapc

let test_input =
  "467..114..\n\
   ...*......\n\
   ..35..633.\n\
   ......#...\n\
   617*......\n\
   .....+.58.\n\
   ..592.....\n\
   ......755.\n\
   ...$.*....\n\
   .664.598.."

type engine = Symbol of char | Value of int | Part of int

let pp_engine fmt e =
  match e with
  | Symbol c -> CCFormat.fprintf fmt "Symbol(%c)" c
  | Value i -> CCFormat.fprintf fmt "Value(%d)" i
  | Part i -> CCFormat.fprintf fmt "Part(%d)" i

let pp_engine_short fmt e =
  match e with
  | Symbol c -> CCFormat.fprintf fmt "%c" c
  | Value i | Part i -> CCFormat.fprintf fmt "%d" i



let make l =
  let f m i j e =
    match e with
    | '.' -> m
    | '0' .. '9' -> MC.add (i, j) (Value (int_of_char e - int_of_char '0')) m
    | _ -> MC.add (i, j) (Symbol e) m
  in
  make_map f l

(* mapi : (key -> 'a -> 'b) -> 'a t -> 'b t *)
let mark_parts m =
  let f_neigh e =
    match e with Symbol _ -> true | Part _ -> false | Value _ -> false
  in
  let f_mapi p e =
    match e with
    | Symbol _ -> e
    | Part _ -> e
    | Value x -> if exists_neigh f_neigh p m then Part x else e
  in
  MC.mapi f_mapi m

let get_parts m =
  let f (current, (i', j'), acc) (i, j) =
    let b = i = i' && j = j' + 1 in
    (* let _ = CCFormat.printf "Debug: (%d,%d), %a %B\n" i j (CCOption.pp pp_engine) current b in *)
    match (MC.get (i, j) m, current, b) with
    | _, Some (Symbol _), _ -> failwith "impossible case"
    | None, _, _ -> failwith "should not be none"
    | Some (Symbol _), None, _ | Some (Symbol _), Some (Value _), _ ->
        (None, (i, j), acc)
    | Some (Symbol _), Some (Part x), _ -> (None, (i, j), x :: acc)
    | x, None, _ -> (x, (i,j), acc)
    | Some (Part v), Some (Part x), false -> (Some (Part v), (i, j), x :: acc)
    | Some (Value v), Some (Part x), false -> (Some (Value v), (i, j), x :: acc)
    | Some (Part v), _, false -> (Some (Part v), (i,j), acc)
    | Some (Value v), _, false -> (Some (Value v), (i,j), acc)
    | Some (Part v), Some (Value v'), true
    | Some (Part v), Some (Part v'), true
    | Some (Value v), Some (Part v'), true ->
        (Some (Part ((v' * 10) + v)), (i, j), acc)
    | Some (Value v), Some (Value v'), true ->
        (Some (Value ((v' * 10) + v)), (i, j), acc)
  in
  let current, _, acc = iter_lines m |> Iter.fold f (None, (-10, -10), []) in
  match current with
  | Some (Part v) -> v::acc
  | _ -> acc

let run input =
  input |> CCString.lines
  |> CCList.map CCString.to_list
  |> make |> mark_parts 
  |> get_parts |> CCList.fold_left ( + ) 0
  |> fun a ->
  CCFormat.printf "Résultat: %d@." a;
  a

let () =
  assert (run test_input == 4361);

  let input = Lwt_main.run (Aoc.input 3) in
  print_int (run input)
