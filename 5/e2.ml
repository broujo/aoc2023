open Helpers
open Parser

let test_input =
  "seeds: 79 14 55 13\n\n\
   seed-to-soil map:\n\
   50 98 2\n\
   52 50 48\n\n\
   soil-to-fertilizer map:\n\
   0 15 37\n\
   37 52 2\n\
   39 0 15\n\n\
   fertilizer-to-water map:\n\
   49 53 8\n\
   0 11 42\n\
   42 0 7\n\
   57 7 4\n\n\
   water-to-light map:\n\
   88 18 7\n\
   18 25 70\n\n\
   light-to-temperature map:\n\
   45 77 23\n\
   81 45 19\n\
   68 64 13\n\n\
   temperature-to-humidity map:\n\
   0 69 1\n\
   1 0 69\n\n\
   humidity-to-location map:\n\
   60 56 37\n\
   56 93 4"

type seed_range = { low : int; high : int }

(* low < high < r.source nothing
   low <= source <= high <= source + length


   r.source + r.length < low < high nothing

   *)

let is_empty s = s.low > s.high
let is_not_empty s = not (is_empty s)

let empty = {low = 1; high = 0}

let rec intersection x y =
  if is_empty y || is_empty y
  then empty
  else
    if x.low > y.low
    then intersection y x
    else
      if x.high < y.low
      then empty
      else {low = y.low; high = min x.high y.high}
      
let rec union x y =
  if is_empty x then [y]
  else
    if x.low < y.low then union y x
    else
      if x.high < y.low
      then [x; y]
      else [{low = x.low; high = max x.high y.high}]

 let sorted_insert ~cmp ?(uniq = false) x l =
  let rec aux cmp uniq x left l =
    match l with
    | [] -> List.rev_append left [ x ]
    | y :: tail ->
      (match cmp x y with
      | 0 ->
        let l' =
          if uniq then
            l
          else
            x :: l
        in
        List.rev_append left l'
      | n when n < 0 -> List.rev_append left (x :: l)
      | _ -> aux cmp uniq x (y :: left) tail)
  in
  aux cmp uniq x [] l     

let union_l x l =
  let rec aux x l acc =
    match l with
    | [] -> CCList.rev_append acc [x]
    | {low; high} :: q when x.low > high -> aux x q ({low; high} :: acc) 
    | {low; high=_} :: _q when x.high < low -> CCList.rev_append acc (x :: l)
    | {low; high} :: q -> let u = {low = min x.low low; high = max x.high high} in aux u q acc
  in
  aux x l []


let minus x y =
  if is_empty y then [x]
  else
    let i = intersection x y in
    if is_empty i then [x]
    else
      [{low = x.low; high = i.low - 1}; {low = i.high + 1; high = x.high}]
      |> CCList.filter is_not_empty

let cut x y =
  let i = intersection x y in
  let m = minus x y in
  let i' = if is_empty i then [] else [i] in
  (i', m)

let cut_l l y =
  let f x (i, m) =
    let i', m' = cut x y in
    (i' @ i, m' @ m) in
  CCList.fold_right f l ([], [])

let reduce_range l =
  CCList.fold_right union_l l []

let do_range r (mapped, rest) =
  let y = { low = r.source; high = r.source + r.length - 1 } in
  let mapped', rest = cut_l rest y in
  let mapped' =
    (CCList.map (fun {low; high} -> {low = low - r.source + r.dest; high = high - r.source + r.dest}) mapped') in
  (mapped' @ mapped), rest
   
let do_range_old n r =
  if n >= r.source && n < (r.source + r.length)
  then
    (n - r.source + r.dest), `Stop
  else
    n, `Continue

let apply_map l m =
  let mapped, rest =
    CCList.fold_right do_range m.ranges ([], l) in
  reduce_range (mapped @ rest)
  
let apply_map_old n m =
  (* CCFormat.printf "%s: %d@ " m.from n; *)
  CCList.fold_while do_range_old n m.ranges

let apply_maps ms l =
  CCList.fold_left apply_map l ms

let apply_maps_old ms n =
  CCList.fold_left apply_map_old n ms

let seeds_to_range seeds =
  let rec aux l acc =
    match l with
    | [] -> acc
    | low :: length :: q -> aux q ({low; high = low + length - 1} :: acc)
    | _ :: [] -> failwith "no range"
  in
  aux seeds []

let run input =
  let seeds, maps = parse input in
  let seeds = seeds_to_range seeds in
  apply_maps maps seeds
  |> reduce_range
  |> CCList.hd |> (fun a -> a.low)

let () =
  assert (run test_input == 46);

  let input = Lwt_main.run (Aoc.input 5) in
  print_int (run input)
