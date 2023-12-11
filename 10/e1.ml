open Helpers
open Helpers.Mapc
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
  
  let pp pp_a fmt m =
    if marked m then CCFormat.fprintf fmt "\x1b[1;31m";
    pp_a fmt (get_value m);
    if marked m then CCFormat.fprintf fmt "\x1b[0m";

end
type tile = NS | EW | NE | NW | SW | SE | Start

let to_char e =
  match e with
  | NS -> '|'
  | EW -> '-'
  | NE -> 'L'
  | NW -> 'J'
  | SW -> '7'
  | SE -> 'F'
  | Start -> 'S'

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

let move (ei, ej) (ti, tj) tile =
  match tile with
  | Start when ei = ti && ej = tj  -> coord_neigh_c_i (ei, ej)
  | Start -> failwith "start entry should be itself"
  | NS when ei + 1 = ti && ej = tj  -> [(ti + 1, tj)]
  | NS when ei - 1 = ti && ej = tj  -> [(ti - 1, tj)]
  | NS -> []

  | EW when ei = ti && ej + 1 = tj -> [(ti, tj + 1)]
  | EW when ei = ti && ej - 1 = tj -> [(ti, tj - 1)]
  | EW -> []

  | NE when ei + 1 = ti && ej = tj  -> [(ti, tj + 1)]
  | NE when ei = ti && ej - 1 = tj -> [(ti - 1, tj)]
  | NE -> []

  | NW when ei + 1 = ti && ej = tj  -> [(ti, tj - 1)]
  | NW when ei = ti && ej + 1 = tj -> [(ti - 1, tj)]
  | NW -> []

  | SW when ei - 1 = ti && ej = tj  -> [(ti, tj - 1)]
  | SW when ei = ti && ej + 1 = tj -> [(ti + 1, tj)]
  | SW -> []

  | SE when ei - 1 = ti && ej = tj  -> [(ti, tj + 1)]
  | SE when ei = ti && ej - 1 = tj -> [(ti + 1, tj)]
  | SE -> []

let make l =
  let f m i j e =
    match from_char e with
    | None -> m
    | Some tile -> MC.add (i, j) tile m
  in make_map f l

let move_m i tile_l m =
  let f i macc (entry_p, tile_p) =
    let _ = CCFormat.printf "Tile: %a@." (CCPair.pp CCInt.pp CCInt.pp) tile_p in
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
  

let run input =
  input |> CCString.lines
  |> CCList.map CCString.to_list
  |> make
  |> explore
  |> fst

let () =
  assert (run test_input == 4);
  assert (run test_input_2 == 8);

  let input = Lwt_main.run (Aoc.input 10) in
  print_int (run input)
