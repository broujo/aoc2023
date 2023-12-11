open Helpers
open Parser

let test_input =
  "LR\n\n\
   11A = (11B, XXX)\n\
   11B = (XXX, 11Z)\n\
   11Z = (11B, XXX)\n\
   22A = (22B, XXX)\n\
   22B = (22C, 22C)\n\
   22C = (22Z, 22Z)\n\
   22Z = (22B, 22B)\n\
   XXX = (XXX, XXX)"

let get_dir node d graph =
  let n = Graph.get node graph |> CCOption.get_exn_or "Error no such node" in
  match d with R -> n.right | L -> n.left

let find_As graph = Graph.keys graph |> Iter.filter (fun k -> k.[2] = 'A')

let find_loop graph dir node =
  let rec find_loop count l (ocur, od) cur =
    match l with
    | [] -> find_loop count dir (ocur, od) cur
    | d :: q ->
        if cur = ocur && (CCList.equal (=) dir od) then count
          else find_loop (count + 1) q (ocur, od) (get_dir cur d graph)
  in
  let rec find_first count l cur =
    match l with
    | [] -> find_first count dir cur
    | d :: q ->
        if cur.[2] = 'Z' then
          let loop = find_loop 1 q (cur, dir) (get_dir cur d graph) in
          (count, loop)
        else find_first (count + 1) q (get_dir cur d graph)
  in
  find_first 0 dir node

let get_n graph node dir n =
  let rec aux n d cur =
    if n = 0
    then cur
    else match d with
    | [] -> aux n dir cur
    | t :: q -> aux (n-1) q (get_dir cur t graph)
  in aux n dir node 

let rec gcd u v = if v <> 0 then gcd v (u mod v) else abs u

let lcm m n =
  match (m, n) with 0, _ | _, 0 -> 0 | m, n -> abs (m * n) / gcd m n

let rec find_common (s1, l1) (s2, l2) =
  if l1 < l2 then find_common (s2, l2) (s1, l1)
  else
    let rec find_k cur k =
      if cur mod l2 = 0 then cur else find_k (cur + l1) (k + 1) in
    let cur = find_k s1 1 in
    let lr = lcm l1 l2 in
    cur, lr


let find_count nodes dir graph =
  let rec aux count l curs =
    if Iter.for_all (fun s -> s.[2] = 'Z') curs then count
    else
      match l with
      | [] -> aux count dir curs
      | d :: q ->
          aux (count + 1) q (Iter.map (fun cur -> get_dir cur d graph) curs)
  in
  aux 0 dir nodes

let run input =
  let dir, graph = parse input in
  let aas = find_As graph in
  let loops = Iter.map (find_loop graph dir) aas in
  Iter.fold find_common (1, 1) loops |> fst

let () =
  assert (run test_input == 6);

  let input = Lwt_main.run (Aoc.input 8) in
  print_int (run input)
