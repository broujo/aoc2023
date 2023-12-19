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

let print_m = CCFormat.printf "%a@." (pp_mc pp_rock)

type dir = N | S | E | W

let tilt_dir_f dir m =
  let (mini,maxi),(minj,maxj) = min_max m in
  let _get_sec = match dir with N | S -> snd | E | W -> fst in
  let get_main = match dir with N | S -> fst | E | W -> snd in
  let set_right (i, _) j = (i, j) in
  let set_left (_, j) i= (i, j) in
  let _set_sec = match dir with N | S -> set_right | E | W -> set_left
  and set_main = match dir with N | S -> set_left | E | W -> set_right in

  let ( --- ) = match dir with N | W -> Iter.Infix.( --^ ) | S | E -> Iter.Infix.( --^  ) in
  let ( ---^ ) = match dir with N | W -> Iter.Infix.( -- ) | S | E -> Iter.Infix.( -- ) in
  let last = match dir with N -> mini | S -> maxi | E -> maxj | W -> minj in
  let first = match dir with N -> maxi | S -> mini | E -> minj | W -> maxj in
  let add = fun i n -> match dir with S | E -> CCInt.sub i n | N | W -> CCInt.add i n in
  get_main, set_main, (---), (---^), last, first, add

  (*
let tilt_dir_line dir p e m =
  let get_main, set_main, (---), (---^), last, first, add =
    tilt_dir_f dir m in

  first --- last
  |> Iter.map (set_main p)
  |> Iter.fold
       (fun (acc, m) p ->
         match MC.get p m with
         | Some Round -> acc + 1, MC.remove p m
         | None -> acc, m
         | Some Cube ->
             add (get_main p) 1 ---^ add (get_main p) 3
             |> CCIter.map (set_main p)
             |> Iter.fold (fun m p -> MC.add p Round m)
             |> fun m -> 0, m)
       m

*)
 
  
let tilt_dir_p dir p e m =
  let get_main, set_main, (---), _, last, _, add =
    tilt_dir_f dir m in

  let next = add 1 in
  match e with
  | Cube -> m
  | Round ->
      let p' =
        next (get_main p) --- last
        |> Iter.map (set_main p)
        |> Iter.fold_while
             (fun acc p ->
               match MC.get p m with
               | None -> (p, `Continue)
               | Some Cube -> (acc, `Stop)
               | Some Round -> (acc, `Continue))
             p
      in m |> MC.remove p |> MC.add p' Round

let tilt_dir dir m = MC.fold (tilt_dir_p dir) m m

let next_dir = function
  | N -> W
  | W -> S
  | S -> E
  | E -> N

let find_repeatition m =
  let mem = Hashtbl.create 100 in
  let rec aux m dir i =
    CCFormat.printf "%d@." i;
    if Hashtbl.mem mem (m, dir) then
      Hashtbl.find mem (m, dir), i
    else
      let _ = Hashtbl.add mem (m, dir) i in
      let m = tilt_dir dir m in
      aux m (next_dir dir) (i + 1) in
  aux m N 0

let rec repeat n dir m =
  CCFormat.printf "%d@." n;
  if n = 0
  then m
  else repeat (n - 1) (next_dir dir) (tilt_dir dir m)

let move n m =
  let n = n * 4 in
  let (i, i') = find_repeatition m in
  CCFormat.printf "%d -- %d@." i i';
  if n < i' then
    repeat n N m
  else
    let base = n - i in
    let di = i' - i in
    let rest = base mod di in
    repeat (rest + i) N m

let load m =
  let (_, maxi), _ = min_max m in
  MC.fold
    (fun (i, _) e acc ->
      match e with Round -> acc + (maxi - i + 1) | _ -> acc)
    m 0

let run input =
  input |> CCString.lines
  |> CCList.map CCString.to_list
  |> make
  |> repeat 20 N
  (* |> move 1000000000 *)
  |> load
  |> tap (CCFormat.printf "%d@.")

let () =
  (* assert (run test_input == 64 ); *)

  let input = Lwt_main.run (Aoc.input 14) in
  print_int (run input)
