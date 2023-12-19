open Helpers
open Helpers.Mapc
open CCFun

let test_input =
  "2413432311323\n\
   3215453535623\n\
   3255245654254\n\
   3446585845452\n\
   4546657867536\n\
   1438598798454\n\
   4457876987766\n\
   3637877979653\n\
   4654967986887\n\
   4564679986453\n\
   1224686865563\n\
   2546548887735\n\
   4322674655533"

type dir = N | S | E | W

let all_dirs = [ N; S; E; W ]
let dir_to_char d =
  match d with
  | N -> 'N'
  | E -> 'E'
  | S -> 'S'
  | W -> 'W'
let pp_dir fmt e = CCFormat.fprintf fmt "%c" (dir_to_char e)

module CG = struct
  type t = C.t * int * dir

  let compare = compare
  let pp fmt ((i,j), _, _) =
    CCFormat.fprintf fmt "(%d, %d)" i j 

end

module G = CCGraph.Map (CG)

let from_char c =
  match c with
  | '0' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> failwith "unknown char"

let make l =
  let f m i j e = MC.add (i, j) (from_char e) m in
  make_map f l

let coord_neigh_dir_i (x, y) =
  [ ((x + 1, y), N); ((x - 1, y), S); ((x, y + 1), W); ((x, y - 1), E) ]

let iter_neigh_dir_i p m =
  Iter.of_list (coord_neigh_dir_i p) |> Iter.filter (fun (p, _) -> MC.mem p m)

let opposite e1 e2 =
  match e1, e2 with
  | N, S | S, N | E, W | W, E -> true
  | _ -> false

let make_graph m =
  let open Iter.Infix in
  let f p e g =
    iter_neigh_dir_i p m
    |> Iter.fold
         (fun g (p', dir) ->
           Iter.product (1 -- 3) (CCList.to_iter all_dirs)
           |> Iter.filter (fun (n, dir') -> not (n = 3 && dir' = dir))
           |> Iter.fold
                (fun g (n, dir') ->
                  if dir = dir' then
                    G.add_edge (p', n, dir') e (p, n + 1, dir) g
                  else if opposite dir dir' && (p' <> (0,0)) then g
                  else G.add_edge (p', n, dir') e (p, 1, dir) g)
                g)
         g
  in
  MC.fold f m G.empty

let pp_path l =
  let pp_aux fmt (_, n, v) =
    CCFormat.fprintf fmt "%a: %d" CG.pp v n in
  let pp_sep = fun fmt () -> CCFormat.fprintf fmt "@." in
  CCList.pp ~pp_sep pp_aux l
  
let run input =
  let m =
    input |> CCString.lines |> CCList.map CCString.to_list |> make
  in let (_mini, maxi), (_minj, maxj) = min_max m in
  m
  |> make_graph
  |> G.as_graph
  |> (fun graph ->
      CCGraph.Traverse.dijkstra
       ~dist:id
       ~tbl:(CCGraph.mk_map ~cmp:compare ())
       ~graph
       (Iter.return ((0, 0), 1, W)))
  |> Iter.find_pred_exn (fun ((p, _, _), _, _) -> p = (maxi, maxj))
  |> (fun (_v, n, _path) -> n)
       
    

let () =
  assert (run test_input == 102);

  let input = Lwt_main.run (Aoc.input 17) in
  print_int (run input)
