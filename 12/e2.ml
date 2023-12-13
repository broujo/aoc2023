open Helpers
open CCFun

let test_input =
  "???.### 1,1,3\n\
   .??..??...?##. 1,1,3\n\
   ?#?#?#?#?#?#?#? 1,3,1,6\n\
   ????.#...#... 4,1,1\n\
   ????.######..#####. 1,6,5\n\
   ?###???????? 3,2,1"

type spring = Operational | Damage | Unknown

let spring_from_char c =
  match c with
  | '?' -> Unknown
  | '.' -> Operational
  | '#' -> Damage
  | _ -> failwith (CCFormat.sprintf "Not a spring: %c" c)

let spring_to_char s =
  match s with
  | Unknown -> '?'
  | Operational -> '.'
  | Damage -> '#'

let pp_spring fmt s = CCChar.pp fmt (spring_to_char s)

let rec solve count solutions =
  if CCList.is_empty solutions then count
  else
    let (l_s, l_c, n), sl = CCList.hd_tl solutions in
    (* cut case when impossible.. probably not worth on small case
       if
         CCList.length l_s
         < CCList.fold_right ( + ) l_c 0 + n + CCList.length l_c - 1
       then count
       else *)
    match (l_s, l_c) with
    (* n > 0 we need Damage.. *)
    | Operational :: _, _ when n > 0 -> solve count sl
    | (Unknown :: lsq, _ | Damage :: lsq, _) when n > 0 ->
        solve count ((lsq, l_c, n - 1) :: sl)
    (* n = 0 we need Operational *)
    | Damage :: _, _ when n = 0 -> solve count sl
    | (Operational :: lsq, _ | Unknown :: lsq, _) when n = 0 ->
        solve count ((lsq, l_c, n - 1) :: sl)
    (* n < 0 *)
    | Operational :: lsq, _ | Unknown :: lsq, [] ->
        solve count ((lsq, l_c, n) :: sl)
    | Damage :: lsq, next_n :: lcq -> solve count ((lsq, lcq, next_n - 1) :: sl)
    | Damage :: _, [] -> solve count sl
    | Unknown :: lsq, next_n :: lcq ->
        let d_case = (lsq, lcq, next_n - 1) in
        let o_case = (lsq, l_c, n) in
        solve count (d_case :: o_case :: sl)
    | [], [] when n > 0 -> solve count sl
    | [], [] -> solve (count + 1) sl
    | [], _ -> solve count sl

exception No_sol

let simplify ls lc =

  let rec aux l_s l_c n =
    if n > 0 then (match (l_s, l_c) with
      | Operational :: _, _ -> raise No_sol
      | Unknown :: lsq, _ | Damage :: lsq, _ -> aux lsq l_c (n - 1)
      | [], _ -> raise No_sol)
    else (match (l_s, l_c) with
      | Damage :: _, _ when n = 0 -> raise No_sol
      | (Operational :: lsq, _ | Unknown :: lsq, _) when n = 0 ->
          aux lsq l_c (n - 1)
      | Operational :: lsq, _ | Unknown :: lsq, [] ->
          aux lsq l_c n
      | Damage :: lsq, next_n :: lcq -> aux lsq lcq (next_n - 1)
      | Damage :: _, [] -> raise No_sol
      | Unknown :: _, _ ->
          (l_s, l_c)
      | [], [] -> (l_s, l_c)
      | [], _ -> raise No_sol) in
  let ls, lc = aux ls lc (-1) in (* from left *)
  let ls' = CCList.rev ls in
  let lc' = CCList.rev lc in
  let ls', lc' = aux ls' lc' (-1) in (* from right *)
  let ls = CCList.rev ls' in
  let lc = CCList.rev lc' in
  ls, lc

let split_list list =
  let open CCList in
  let n = length list in
  if n = 0 || n = 1 then failwith "cant split that" else
  let n = n/2 in
  let rec aux l n acc =
    if n <= 0 then (rev acc), l else
    match l with
    | [] -> failwith "bug"
    | t :: q -> aux q (n-1) (t::acc) in
  aux list n []

let min_size c_1 =
  (CCList.reduce_exn (+) c_1) + (CCList.length c_1) - 1

let count springs conditions = solve 0 [(springs, conditions, -1)]

let rec solve_by_divide springs conditions =
  (* let _ = CCFormat.printf "Solve %s %a@." (CCString.of_list (CCList.map spring_to_char springs)) (CCList.pp ~pp_sep:(fun fmt () -> CCFormat.fprintf fmt ", ") CCInt.pp) conditions in *)
  let springs, conditions = simplify springs conditions in
  if CCList.is_empty springs && CCList.is_empty conditions
  then 1
  else
    if CCList.length conditions = 1 then
      let s = solve 0 [(springs, conditions, -1)] in
      if s = 0 then raise No_sol
      else s
    else
    let c1, c2 = split_list conditions in
    let ms1 = min_size c1 in
    let ms2 = min_size c2 in
    let total_size = CCList.length springs in
    
    (* now i need to position ms1, then try all ms2 positions *)

    let rec aux_2 r1 sp2 s acc =
      match sp2 with
      | [] -> acc
      | _ when s + ms2 > total_size -> acc
      | Operational :: q -> aux_2 r1 q (s + 1) acc
      | Damage :: _ ->
          let acc' =
            try
              let r2 = solve_by_divide sp2 c2 in
              r1 * r2
            with No_sol -> 0
          in acc + acc'
      | Unknown :: q ->
          let acc' =
            try
              let r2 = solve_by_divide (Damage :: q) c2 in
              r1 * r2
            with No_sol -> 0
          in aux_2 r1 q (s + 1) (acc + acc') in

    let rec aux_1 sp1 springs s acc =
      match springs with
      | [] -> acc
      | _ when s + ms2 > total_size -> acc
      | t::q when s < ms1 - 1 -> aux_1 (t::sp1) q (s + 1) acc
      | (Operational as t) :: q -> aux_1 (t::sp1) q (s + 1) acc
      | (Unknown as t1) :: (Damage as t2) :: q
      | (Damage as t1) :: (Damage as t2) :: q -> aux_1 (t1 :: sp1) (t2 :: q) (s + 1) acc
      | (Unknown as t1) :: t2 :: q | (Damage as t1) :: t2 :: q ->
          let r1 =
            try solve_by_divide (CCList.rev (Damage :: sp1)) c1
            with No_sol -> 0
          in
          let acc =
            if r1 = 0 then acc
            else aux_2 r1 q (s + 1) acc in
          aux_1 (t1 :: sp1) (t2 :: q) (s + 1) acc
      | _ :: [] -> acc in
    let r = aux_1 [] springs 0 0 in
    if r = 0 then raise No_sol else r

  

let parse ?(repeat=5) line =
  let spring, condition =
    match CCString.split ~by:" " line with
    | [ a; b ] -> (a, b)
    | _ -> failwith (CCFormat.sprintf "bad input: <%s>" line)
  in
  let spring =
    CCString.to_list spring |> CCList.map spring_from_char
    |> CCList.return
    |> CCList.repeat repeat
    |> CCList.intersperse [Unknown]
    |> CCList.flatten
    (*|> CCList.group_succ ~eq:( = )
      |> CCList.map (fun l -> (CCList.hd l, CCList.length l))*)
  in
  let condition =
    CCString.split ~by:"," condition |> CCList.map int_of_string
    |> CCList.repeat repeat
  in

  (spring, condition)

let run input = input |> CCString.lines |> CCList.map parse |> CCList.map (uncurry solve_by_divide) |> CCList.reduce_exn (+)

let () =
  assert (run test_input == 525152);

  let input = Lwt_main.run (Aoc.input 12) in
  print_int (run input)
