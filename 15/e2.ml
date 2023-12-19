open Helpers
open Parser

let test_input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

module M = CCMap.Make (CCInt)

let pp_iter i =
  let pp_start out () = CCFormat.fprintf out "[" in
  let pp_stop out () = CCFormat.fprintf out "]" in
  let pp_sep out () = CCFormat.fprintf out "" in
  let pp_s fmt s = CCFormat.fprintf fmt "%s" s in

  Iter.pp_seq ~sep:" " (CCPair.pp ~pp_start ~pp_stop ~pp_sep pp_s CCInt.pp) i

let op_to_s o = match o with Dash -> "-" | Equal i -> CCFormat.sprintf "=%d" i
let pp_op fmt o = CCFormat.fprintf fmt "%s" (op_to_s o)

let pp_print m =
  let pp_start out () = CCFormat.fprintf out "@[Box " in
  let pp_stop out () = CCFormat.fprintf out "@]" in
  let pp_sep out () = CCFormat.fprintf out "@]@.@[Box " in
  let pp_arrow out () = CCFormat.fprintf out ": " in
  M.pp ~pp_start ~pp_stop ~pp_sep ~pp_arrow CCInt.pp pp_iter m

let hash_step current c = (current + CCChar.to_int c) * 17 mod 256
let hash s = CCString.fold hash_step 0 s

let hashmap_step m ((label, hash), op) =
  (* CCFormat.printf "%a" pp_print m;
     CCFormat.printf "@.After \"%s%a\":@." label pp_op op; *)
  let open Iter in
  match op with
  | Dash ->
      M.update hash
        (function
          | None -> None | Some i -> Some (filter (fun (l, _) -> l <> label) i))
        m
  | Equal n ->
      M.update hash
        (function
          | None -> Some (return (label, n))
          | Some i ->
              if mem ~eq:(fun (a, _) (b, _) -> a = b) (label, 1) i then
                Some
                  (map (fun (a, b) -> if a = label then (a, n) else (a, b)) i)
              else Some (snoc i (label, n)))
        m

let hashmap l = CCList.fold_left hashmap_step M.empty l

let focusing_power m =
  let f box iter acc =
    Iter.foldi
      (fun acc slot (_label, focal) -> ((1 + box) * focal * (slot + 1)) + acc)
      acc iter
  in
  M.fold f m 0

let run input =
  input |> CCString.lines
  |> CCList.map (CCString.split_on_char ',')
  |> CCList.flatten |> CCList.map parse
  |> CCList.map (CCPair.map_fst (fun s -> (s, hash s)))
  |> hashmap |> focusing_power

let () =
  assert (run test_input = 145);

  let input = Lwt_main.run (Aoc.input 15) in
  print_int (run input)
