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

let count springs conditions = solve 0 [(springs, conditions, -1)]

let parse line =
  let spring, condition =
    match CCString.split ~by:" " line with
    | [ a; b ] -> (a, b)
    | _ -> failwith (CCFormat.sprintf "bad input: <%s>" line)
  in
  let spring =
    CCString.to_list spring |> CCList.map spring_from_char
    (*|> CCList.group_succ ~eq:( = )
      |> CCList.map (fun l -> (CCList.hd l, CCList.length l))*)
  in
  let condition =
    CCString.split ~by:"," condition |> CCList.map int_of_string
  in

  (spring, condition)

let run input = input |> CCString.lines |> CCList.map parse |> CCList.map (uncurry count) |> CCList.reduce_exn (+)

let () =
  assert (run test_input == 21);

  let input = Lwt_main.run (Aoc.input 12) in
  print_int (run input)
