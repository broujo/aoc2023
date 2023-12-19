open Helpers
open Helpers.Mapc
open CCFun

let test_input = ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...."

type dir = N | E | S | W

module MemS = struct
  type t = C.t * dir
  let compare = compare
end

module S = CCSet.Make (C)
module Mem = CCSet.Make (MemS)

let all_dir = [N; E; S; W]
type grid = Mirror | MirrorBack | Vert | Hori

let to_char e =
  match e with
  | Mirror -> '/'
  | MirrorBack -> '\\'
  | Vert -> '|'
  | Hori -> '-'

let from_char e =
  match e with
  | '/' -> Some Mirror
  | '-' -> Some Hori
  | '|' -> Some Vert
  | '\\' -> Some MirrorBack
  | '.' -> None
  | _ -> failwith (CCFormat.sprintf "impossible: %c" e)

let pp_tile fmt e =
  CCFormat.fprintf fmt "%c" (to_char e)

let make l =
  let f m i j e =
    match from_char e with
    | None -> m
    | Some tile -> MC.add (i, j) tile m
  in make_map f l

let find_neigh_dir p m dir =
  let open Iter.Infix in
  let (mini, maxi), (minj, maxj) = min_max m in
  let set_fst (_i, j) i' = (i', j) in
  let set_snd (i, _j) j' = (i, j') in

  let do_dir get set maxx (--) dir =
    let next =
      (get p) -- maxx
      |> Iter.drop 1
      |> Iter.map (set p)
      |> Iter.find (fun p -> MC.get p m |> CCOption.map (fun _ -> p)) in
    match next with
    | None -> []
    | Some p' -> [(dir, p')] in

  match dir with
  | S -> do_dir fst set_fst maxi (--) S
  | N -> do_dir fst set_fst mini (--^) N
  | W -> do_dir snd set_snd minj (--^) W
  | E -> do_dir snd set_snd maxj (--) E


let map_neigh m =
  let mn = MC.fold (fun p _ m' -> MC.add p (CCList.flat_map (find_neigh_dir p m) all_dir) m') m MC.empty
  in
  MC.update (0,0) (function
  | None -> Some (find_neigh_dir (0,0) m E)
  | Some l -> Some l) mn
  

let get_energized p p' =
  let open Iter.Infix in
  match p, p' with
  | (i, j), (i', j') when i = i' ->
      let it = if j < j' then (j -- j') else (j' -- j) in
      it |> Iter.map (fun j -> (i,j))
  | (i, j), (i', j') when j = j' ->
      let it = if i < i' then (i -- i') else (i' -- i) in
      it |> Iter.map (fun i -> (i,j))
  | _, _ -> failwith "unknown case"


let beam_path mn m =
  let start = (0, 0) in
  let dir = E in
  let (mini, maxi), (minj, maxj) = min_max mn in
  let open CCOption.Infix in

  let rec energize ((i,j) as p) dir s =
    (match MC.get p mn >>= List.assoc_opt dir with
    | None ->
        (match dir with
        | E -> get_energized p (i, maxj)
        | W -> get_energized p (i, minj)
        | S -> get_energized p (maxi, j)
        | N -> get_energized p (mini, j))
                
    | Some (p') -> get_energized p p')
    |> Iter.fold (fun s p -> S.add p s) s
  and path l s mem =
    let f (s, mem) (p, dir) =
      if Mem.mem (p,dir) mem then (s, mem), []
      else
        let mem = Mem.add (p, dir) mem in
        let dirl =
          (match MC.get p m, dir with
          | None, _
          | Some Vert, S
          | Some Vert, N
          | Some Hori, E
          | Some Hori, W -> [dir]
          | Some Mirror, E
          | Some MirrorBack, W -> [N]
          | Some Mirror, W
          | Some MirrorBack, E -> [S]
          | Some Mirror, N
          | Some MirrorBack, S -> [E]
          | Some Mirror, S
          | Some MirrorBack, N -> [W]
          | Some Vert, W | Some Vert, E -> [N; S]
          | Some Hori, N | Some Hori, S -> [E; W]) in
        let s = CCList.fold_left (fun s d -> energize p d s) s dirl in
        let l = CCList.flat_map (fun d ->
          MC.get p mn
          >|= CCList.filter (fun (d',_ )-> d=d')
          >|= CCList.map CCPair.swap
          |> CCOption.get_or ~default:[]) dirl in
        (s, mem), l in
    CCList.fold_flat_map f (s, mem) l in

  let rec aux s l mem =
    match l with
    | [] -> s
    | _ -> let (s, mem), l = path l s mem in aux s l mem in 
  aux S.empty [start, dir] Mem.empty






(* m = (i,j) -> grid
  mn = (i,j) -> [(dir, (i', j');...]

  (0,0) R ->*)

let run input =
  let m = input |> CCString.lines |> CCList.map CCString.to_list |> make in
  let mn = map_neigh m in
  beam_path mn m |> S.cardinal

let () =
  assert (run test_input == 46);

  let input = Lwt_main.run (Aoc.input 16) in
  print_int (run input)
