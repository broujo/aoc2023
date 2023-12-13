open Helpers
open Mapc
open CCFun

let test_input =
  "#.##..##.\n\
   ..#.##.#.\n\
   ##......#\n\
   ##......#\n\
   ..#.##.#.\n\
   ..##..##.\n\
   #.#.##.#.\n\n\
   #...##..#\n\
   #....#..#\n\
   ..##..###\n\
   #####.##.\n\
   #####.##.\n\
   ..##..###\n\
   #....#..#"

module S = CCSet.Make (C)

let make l =
  let f s i j e =
    match e with
    | '.' -> s
    | '#' -> S.add (i, j) s
    | _ -> failwith (CCFormat.sprintf "Unknown: %c" e)
  in
  CCList.foldi
    (fun m i col -> CCList.foldi (fun m j e -> f m i j e) m col)
    S.empty l

let min_max s =
  S.fold
    (fun (i, j) ((mini, minj), (maxi, maxj)) ->
      ((min i mini, min j minj), (max i maxi, max j maxj)))
    s
    ((CCInt.max_int, CCInt.max_int), (0, 0))

type reflection = H of int | V of int

(* fun p e -> p *)
let find_sym_left ?(get_left = fst) ?(get_right = snd)
    ?(set_left = fun l (_, r) -> (l, r)) ?(set_right = fun r (l, _) -> (l, r))
    ?(switch = false) s =
  let get_left, get_right, set_left, set_right =
    if switch
    then get_right, get_left, set_right, set_left
    else get_left, get_right, set_left, set_right in

  let minp, maxp = min_max s in
  
  let open Iter.Infix in

  (get_left minp) + 1 -- (get_left maxp)
    |> Iter.fold (fun (m_left, diff_left) left ->
        let max_dleft = min ((get_left maxp) - left) (left - (get_left minp) - 1) in
        let found, total =
          0 -- max_dleft
          |> Iter.fold (fun (acc, total) dleft ->
              (get_right minp) -- (get_right maxp)
              |> Iter.fold (fun (acc, total) right ->
                  let p1 = (0,0) |> set_left (left - dleft - 1) |> set_right right in
                  let p2 = (0,0) |> set_left (left + dleft) |> set_right right in
                  if S.mem p1 s = S.mem p2 s then (acc + 1, total + 1)
                  else (acc, total + 1))
              (acc, total))
          (0, 0) in
        if diff_left > total - found && total - found <> 0 then (left, total - found)
        else m_left, diff_left)
    (-1, CCInt.max_int)

let check_sym input = 
  let s = CCString.lines input |> CCList.map CCString.to_list |> make in
  let find_i = find_sym_left s in
  let find_j = find_sym_left ~switch:true s in
  match (find_i, find_j) with
  | (_i, _), (j, 1) -> V j
  | (i, 1), (_j, _) -> H i
  | (i, di), (j, dj) ->
      failwith (CCFormat.sprintf "(%d, %d), (%d, %d) -> @.%s" i di j dj input)

(*
let check_sym_old input =
  let s = CCString.lines input |> CCList.map CCString.to_list |> make in
  let (mini, minj), (maxi, maxj) = min_max s in
  let open Iter.Infix in
  let find_i = 
    mini + 1 -- maxi
    |> Iter.fold
         (fun (mi, diffi) i ->
           let max_di = min (maxi - i) (i - mini - 1) in
           let found, total =
             0 -- max_di
             |> Iter.fold
                  (fun (acc, total) di ->
                    minj -- maxj
                    |> Iter.fold
                         (fun (acc, total) j ->
                           CCFormat.printf "(%d, %d), (%d, %d) Sym %d@."
                             (i - di - 1)
                             j (i + di) j i;
                           if S.mem (i - di - 1, j) s = S.mem (i + di, j) s then
                             (acc + 1, total + 1)
                           else (acc, total + 1))
                         (acc, total))
                  (0, 0)
           in
           if diffi > total - found && total - found <> 0 then (i, total - found)
           else (mi, diffi))
         (-1, CCInt.max_int)
  in
  let find_j =
    minj + 1 -- maxj
    |> Iter.fold
         (fun (mj, diffj) j ->
           let max_dj = min (maxj - j) (j - minj - 1) in
           let found, total =
             0 -- max_dj
             |> Iter.fold
                  (fun (acc, total) dj ->
                    mini -- maxi
                    |> Iter.fold
                         (fun (acc, total) i ->
                           CCFormat.printf "(%d, %d), (%d, %d) Sym %d@." i
                             (j - dj - 1)
                             i (j + dj) i;
                           if S.mem (i, j - dj - 1) s = S.mem (i, j + dj) s then
                             (acc + 1, total + 1)
                           else (acc, total + 1))
                         (acc, total))
                  (0, 0)
           in
           if diffj > total - found && total - found <> 0 then (j, total - found)
           else (mj, diffj))
         (-1, CCInt.max_int)
  in
  match (find_i, find_j) with
  | (_i, _), (j, 1) -> V j
  | (i, 1), (_j, _) -> H i
  | (i, di), (j, dj) ->
      failwith (CCFormat.sprintf "(%d, %d), (%d, %d) -> @.%s" i di j dj input)
*)

let score sym = match sym with H n -> 100 * n | V n -> n

let run input =
  input |> CCString.split ~by:"\n\n" |> CCList.map check_sym |> CCList.map score
  |> CCList.reduce_exn ( + )

let () =
  assert (run test_input == 400);

  let input = Lwt_main.run (Aoc.input 13) in
  print_int (run input)
