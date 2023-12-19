open Helpers
open CCFun

let test_input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

let expand ?(size=1) l =
  let rec aux_i i add li acc =
    match li with
    | [] -> acc
    | (i', j) :: q ->
        if i' > i
        then 
          let add' = add + (i' - i)*(size - 1) in
          aux_i (i' + 1) add' q ((i' + add', j)::acc)
        else if i = i'
        then aux_i (i + 1) add q ((i' + add, j)::acc)
        else aux_i i add q ((i' + add, j)::acc) in
  let rec aux_j j add lj acc =
    match lj with
    | [] -> acc
    | (i, j') :: q ->
        if j' > j
        then 
          let add' = add + (j' - j)*(size - 1) in
          aux_j (j' + 1) add' q ((i, j' + add')::acc)
        else if j = j'
        then aux_j (j + 1) add q ((i, j' + add)::acc)
        else aux_j j add q ((i, j' + add)::acc)
  in
  let li = CCList.sort (fun a b -> CCInt.compare (fst a) (fst b)) l in
  let lj =
    aux_i 0 0 li []
    |> CCList.sort (fun a b -> CCInt.compare (snd a) (snd b)) in
  aux_j 0 0 lj []
  
let shortest_path (ai, aj) (bi, bj) =
  CCInt.abs (ai - bi) + CCInt.abs (aj - bj)

let sum_path l =
  CCList.diagonal l
  |> CCList.map (uncurry shortest_path)
  |> CCList.reduce_exn (+)

let make l =
  let f l i j e =
    match e with
    | '.' -> l
    | '#' -> (i, j) :: l
    | _ -> failwith (CCFormat.sprintf "Unknown: %c" e)
  in
  CCList.foldi
    (fun m i col -> CCList.foldi (fun m j e -> f m i j e) m col)
    [] l

let run size input =
  input |> CCString.lines
  |> CCList.map CCString.to_list
  |> make
  |> expand ~size:size
  |> sum_path

let () =
  assert (run 10 test_input = 1030);
  assert (run 100 test_input = 8410);

  let input = Lwt_main.run (Aoc.input 11) in
  print_int (run 1000000 input)
