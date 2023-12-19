open Helpers
open Mapc
open CCFun

let test_input =
  "O....#....\n\
   O.OO#....#\n\
   .....##...\n\
   OO.#O....O\n\
   .O.....O#.\n\
   O.#..O.#.#\n\
   ..O..#O..O\n\
   .......O..\n\
   #....###..\n\
   #OO..#...."

type rock = Round | Cube

let from_char c =
  match c with
  | 'O' -> Some Round
  | '#' -> Some Cube
  | '.' -> None
  | _ -> failwith (CCFormat.sprintf "Not a roch: %c" c)

let to_char r = match r with Round -> 'O' | Cube -> '#'
let pp_rock fmt e = CCFormat.fprintf fmt "%c" (to_char e)

let make l =
  let f m i j e =
    match from_char e with None -> m | Some t -> MC.add (i, j) t m
  in
  make_map f l

let tilt_north_p (i, j) e m =
  let open Iter.Infix in
  match e with
  | Cube -> m
  | Round ->
    let i' =
      i - 1 --^ 0
      |> Iter.fold_while
           (fun acc i ->
             match MC.get (i, j) m with
             | None -> (i, `Continue)
             | Some Cube -> (acc, `Stop)
             | Some Round -> (acc, `Continue))
           i
    in
    m |> MC.remove (i, j) |> MC.add (i', j) Round

let tilt_north m =
  MC.fold tilt_north_p m m

let load m =
  let (_, maxi), _ = min_max m in
  MC.fold (fun (i, _) e acc ->
    match e with
    | Round -> acc + (maxi - i + 1)
    | _ -> acc) m 0
    
let run input =
  input |> CCString.lines |> CCList.map CCString.to_list |> make
  |> tilt_north
  (* |> tap (CCFormat.printf "%a@." (pp_mc pp_rock)) *)
  |> load
  |> tap (CCFormat.printf "%d@.")

let () =
  assert (run test_input == 136);

  let input = Lwt_main.run (Aoc.input 14) in
  print_int (run input)
