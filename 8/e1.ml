open Helpers
open Parser

let test_input = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

let get_dir node d graph =
  let n = Graph.get node graph |> CCOption.get_exn_or "Error no such node" in
  match d with
  | R -> n.right
  | L -> n.left

let find_count node dir graph =
  let rec aux count l cur =
    if cur = "ZZZ" then count
    else
    match l with
    | [] -> aux count dir cur
    | d :: q -> aux (count + 1) q (get_dir cur d graph)
  in aux 0 dir node

let run input =
  input
  |> parse
  |> (fun (dir,graph) -> find_count "AAA" dir graph) 

let () =
  assert (run test_input == 2);

  let input = Lwt_main.run (Aoc.input 8) in
  print_int (run input)
