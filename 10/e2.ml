open Helpers
open Mapc
open CCFun.Infix

let test_input = ".....
.S-7.
.|.|.
.L-J.
....."

let test_input_2 = "7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ"

let test_input_22 = ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."

module Mark = struct
  type ('a, 'b) t = M of ('a * 'b) | U of 'a
  let return a = U a
  let mark a b = M (a, b)

  let do_mark b m = match m with
  | M (a, _) | U a -> M (a, b)

  let get_value m = match m with
  | M (a, _) | U a -> a

  let marked m = match m with | M (_, _) -> true | U _ -> false
  let unknown m = return m
  let unknowed m = not @@ marked m
  let map_mark f m = match m with M (a, b) -> M (a, f b) | U a -> U a

  let pp pp_a fmt m =
    if marked m then CCFormat.fprintf fmt "\x1b[1m";
    pp_a fmt (get_value m);
    if marked m then CCFormat.fprintf fmt "\x1b[0m";


end
type tile = NS | EW | NE | NW | SW | SE | Start | Ground

let to_char e =
  match e with
  | NS -> '|'
  | EW -> '-'
  | NE -> 'L'
  | NW -> 'J'
  | SW -> '7'
  | SE -> 'F'
  | Start -> 'S'
  | Ground -> '.'

let from_char e =
  match e with
  | '|' -> Some NS
  | '-' -> Some EW
  | 'L' -> Some NE
  | 'J' -> Some NW
  | '7' -> Some SW
  | 'F' -> Some SE
  | 'S' -> Some Start
  | '.' -> None
  | _ -> failwith (CCFormat.sprintf "impossible: %c" e)

let pp_tile fmt e =
  CCFormat.fprintf fmt "%c" (to_char e)

(* should have used direction instead of previous tile.. *)
let move ?(right=false) (ei, ej) (ti, tj) tile =
  let tile', r = match tile with
  | Start when ei = ti && ej = tj  -> coord_neigh_c_i (ei, ej), []
  | Start -> failwith "start entry should be itself"
  | NS when ei + 1 = ti && ej = tj  -> [(ti + 1, tj)], [(ti, tj - 1)]
  | NS when ei - 1 = ti && ej = tj  -> [(ti - 1, tj)], [(ti, tj + 1)]
  | NS -> [], []

  | EW when ei = ti && ej + 1 = tj -> [(ti, tj + 1)], [(ti + 1), tj]
  | EW when ei = ti && ej - 1 = tj -> [(ti, tj - 1)], [(ti - 1), tj]
  | EW -> [], []

  | NE when ei + 1 = ti && ej = tj  -> [(ti, tj + 1)], [(ti, tj - 1); (ti + 1, tj - 1); (ti + 1, tj)]
  | NE when ei = ti && ej - 1 = tj -> [(ti - 1, tj)], []
  | NE -> [], []

  | NW when ei + 1 = ti && ej = tj  -> [(ti, tj - 1)], []
  | NW when ei = ti && ej + 1 = tj -> [(ti - 1, tj)], [(ti + 1, tj); (ti + 1, tj + 1); (ti, tj + 1)]

  | NW -> [], []

  | SW when ei - 1 = ti && ej = tj  -> [(ti, tj - 1)], [(ti, tj + 1); (ti - 1, tj + 1); (ti - 1, tj)]
  | SW when ei = ti && ej + 1 = tj -> [(ti + 1, tj)], []
  | SW -> [], []

  | SE when ei - 1 = ti && ej = tj  -> [(ti, tj + 1)], []
  | SE when ei = ti && ej - 1 = tj -> [(ti + 1, tj)], [(ti - 1, tj); (ti - 1, tj - 1); (ti, tj - 1)]

  | SE -> [], []

  | Ground -> failwith "no ground allowed" in
  if right then tile' @ r else tile'

let make l =
  let f m i j e =
    match from_char e with
    | None -> m
    | Some tile -> MC.add (i, j) tile m
  in make_map f l

let move_m i tile_l m =
  let f i macc (entry_p, tile_p) =
    (* let _ = CCFormat.printf "Tile: %a@." (CCPair.pp CCInt.pp CCInt.pp) tile_p in *)
    match MC.get tile_p macc with
    | None -> (macc, [])
    | Some (Mark.U e) ->
        let tile' = move entry_p tile_p e |> CCList.map (fun p -> tile_p, p) in
        if CCList.is_empty tile' then (macc, tile')
        else
          let macc = MC.update tile_p (CCOption.map (Mark.do_mark i)) macc in
          (macc, tile')
    | Some (Mark.M (e, j)) when i < j ->
        let tile' = move entry_p tile_p e |> CCList.map (fun p -> tile_p, p) in
        let macc = MC.update tile_p (CCOption.map (Mark.do_mark i)) macc in
        (macc, tile')

    | Some (Mark.M (_, _)) -> (macc, []) in

 CCList.fold_flat_map (f i) m tile_l

let find_s m =
  MC.to_iter m |> Iter.find_pred_exn (fun (_,s) -> s = Start) |> fst

let mark_map m = MC.map Mark.return m

let explore m =
  let mm = mark_map m in
  let start = find_s m in
  let rec aux i tl mm =
    match tl with
    | [] -> i - 2, mm (* run once to find mark again, and once to find empty here *)
    | _ -> let mm', tl' = move_m i tl mm in aux (i+1) tl' mm'
  in aux 0 [start, start] mm
  
(* follow inside..
   1. Change S to the right value
   2. get left most
   3. follow the path on the right
   4. mark everything that isn't marked on the right side..
    going up thats right
    going down thats left...
   5. do it until we reach 2.
   6. count the one marked.
   7. profit
   *)

let real_s m =
  let mm = mark_map m in
  let (si, sj) as start = find_s m in
  let mm', l = move_m 0 [start, start] mm in
  let _, l' = move_m 1 l mm' in
  (* change by x before change by y.. *)
  let tile =
    match l' with
  | ((_ei, ej), _) :: ((_fi, fj), _) :: [] when ej = fj -> NS
  | ((ei, _ej), _) :: ((fi, _fj), _) :: [] when ei = fi -> EW
  | ((ei, _ej), _) :: ((_fi, fj), _) :: [] when si - 1 = ei && sj + 1 = fj -> NE 
  | ((ei, _ej), _) :: ((_fi, fj), _) :: [] when si + 1 = ei && sj + 1 = fj -> SE 
  | ((ei, _ej), _) :: ((_fi, fj), _) :: [] when si - 1 = ei && sj - 1 = fj -> NW 
  | ((ei, _ej), _) :: ((_fi, fj), _) :: [] when si + 1 = ei && sj - 1 = fj -> SW 
  | _ -> failwith "ambitious failwith" in
  start, tile


type mark =
  | PathVisited
  | Path
  | Inside

let pp_mark pp_a fmt m =
  let _ = match m with
  | Mark.M (_, PathVisited) -> CCFormat.fprintf fmt "\x1b[92m";
  | Mark.M (_, Path) -> CCFormat.fprintf fmt "\x1b[34m";
  | Mark.M (_, Inside) -> CCFormat.fprintf fmt "\x1b[32m";
  | _ -> () in
  pp_a fmt (Mark.get_value m);
  if Mark.marked m then CCFormat.fprintf fmt "\x1b[0m"

let move_inside l m =
  let f macc (entry_p, tile_p) =
    match MC.get tile_p macc with
    | None ->
        let macc = MC.add tile_p (Mark.M(Ground, Inside)) macc in
        let tile' = coord_neigh_c_i tile_p |> CCList.map (fun p -> tile_p, p) in
        (macc, tile')
    | Some (Mark.U _) ->
        let macc = MC.update tile_p (CCOption.map (Mark.do_mark Inside)) macc in
        let tile' = coord_neigh_c_i tile_p |> CCList.map (fun p -> tile_p, p) in
        (macc, tile')
    | Some (Mark.M (_, PathVisited)) | Some (Mark.M (_, Inside)) -> (macc, [])
    | Some (Mark.M (e, Path)) ->
        let tile' = move ~right:true entry_p tile_p e |> CCList.map (fun p -> tile_p, p) in
        if CCList.is_empty tile' then (macc, [])
        else
          let macc = MC.update tile_p (CCOption.map (Mark.do_mark PathVisited)) macc in
          macc, tile'
  in
  CCList.fold_flat_map f m l

let find_inside animate m =
  let fs = CCFormat.get_formatter_out_functions () in
  if animate then
    CCFormat.set_formatter_output_functions fs.CCFormat.out_string (fun () -> ());
  let _p, s = real_s m in
  let _, mm = explore m in
  let mm = MC.map (function Mark.M (Start, 0) -> Mark.M (s, 0) | e -> e) mm in
  let mm = MC.map (Mark.map_mark (fun _ -> Path)) mm in

  let (si, sj), e = MC.min_binding (MC.filter (fun _ e ->
    match e with
    | Mark.M (_, Path) -> true
    | _ -> false) mm) in

  let _e =
    match e with
    | Mark.M (SE, Path) -> SE
    | _ -> failwith "bad assumption" in
  let mm = MC.update (si, sj) (CCOption.map (Mark.do_mark PathVisited)) mm in
  let rec aux tl mm =
    if animate then
      begin
      CCFormat.printf "\x1b[2J\x1b[H";
      CCFormat.printf "%a@." (pp_mc (pp_mark pp_tile)) mm;
      fs.CCFormat.out_flush ();
      Unix.sleepf 0.2;
      end;
    match tl with
    | [] -> mm
    | _ -> let mm', tl' = move_inside tl mm in aux tl' mm' in
  aux [((si, sj),(si, sj + 1))] mm

let count_inside mm =
  let f e =
    match e with
    | Mark.M (_, Inside) -> true
    | _ -> false
  in
  MC.fold (fun _p e acc -> if f e then acc + 1 else acc) mm 0

let run input =
  input |> CCString.lines
  |> CCList.map CCString.to_list
  |> make
  |> find_inside false
  |> count_inside

let () =
  assert (run test_input_22 == 8);

  let input = Lwt_main.run (Aoc.input 10) in
  print_int (run input)
