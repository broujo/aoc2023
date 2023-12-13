open Helpers
open Mapc
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

let test_input = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

module S = CCSet.Make(C)

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
  S.fold (fun (i,j) ((mini, minj), (maxi, maxj)) -> ((min i mini, min j minj), (max i maxi, max j maxj))) s ((CCInt.max_int, CCInt.max_int), (0,0))

type reflection =
  | H of int
  | V of int

let check_sym input =
  let s = CCString.lines input |> CCList.map CCString.to_list |> make in
  let (mini, minj), (maxi, maxj) = min_max s in
  let open Iter.Infix in
  let find_i =
    (mini + 1) -- maxi
    |> Iter.find_pred (fun i ->
        let max_di = min (maxi - i) (i - mini - 1) in
        0 -- max_di
        |> Iter.for_all (fun di ->
            minj -- maxj
            |> Iter.for_all (fun j ->
                CCFormat.printf "(%d, %d), (%d, %d) Sym %d@." (i - di - 1) j (i + di) j i;
                S.mem (i - di - 1,j) s = S.mem (i + di, j) s
            )
        )
    ) in
  let find_j =
    (minj + 1) -- maxj
    |> Iter.find_pred (fun j ->
        let max_dj = min (maxj - j) (j - minj - 1) in
        0 -- max_dj
        |> Iter.for_all (fun dj ->
            mini -- maxi
            |> Iter.for_all (fun i ->
                CCFormat.printf "(%d, %d), (%d, %d) Sym %d@." i (j - dj - 1) i (j + dj) j;
                S.mem (i,j - dj - 1) s = S.mem (i, j + dj) s
            )
        )
    ) in
  match find_i, find_j with
  | None, None -> failwith (input)
  | Some i, None -> H i
  | None, Some j -> V j
  | _, _ -> failwith "Two sym"

let score sym =
  match sym with
  | H n -> 100 * n
  | V n -> n

let run input =
  input |> CCString.split ~by:"\n\n" 
  |> CCList.map check_sym
  |> CCList.map score
  |> CCList.reduce_exn (+)

let () =
  assert (run test_input == 405); 

  let input = Lwt_main.run (Aoc.input 13) in
  print_int (run input)
