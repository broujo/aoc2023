module C = struct
  type t = CCInt.t * CCInt.t

  let compare (x0, y0) (x1, y1) =
    match CCInt.compare x0 x1 with 0 -> CCInt.compare y0 y1 | c -> c
end

module MC = CCMap.Make (C)

let make_map f l =
  CCList.foldi
    (fun m i col -> CCList.foldi (fun m j e -> f m i j e) m col)
    MC.empty l

let min_max m =
  let minx, miny =
    MC.fold
      (fun (x, y) _ (mx, my) -> (min mx x, min my y))
      m
      (CCInt.max_int, CCInt.max_int)
  in

  let maxx, maxy =
    MC.fold
      (fun (x, y) _ (mx, my) -> (max mx x, max my y))
      m
      (CCInt.min_int, CCInt.min_int)
  in

  ((minx, maxx), (miny, maxy))

let iter_all_lines m =
  let open Iter.Infix in
  let (minx, maxx), (miny, maxy) = min_max m in
  minx -- maxx
  >>= (fun i -> miny -- maxy >|= fun j -> (i, j))


let iter_lines m =
  iter_all_lines m |> Iter.filter (fun p -> MC.mem p m)

(* CCFormat.fprintf fmt "%s" (engine_to_s e) *)

let pp_mc ?(pp_none = fun fmt () -> Format.fprintf fmt ".") pp_v fmt m =
  let open Iter.Infix in
  let (minx, maxx), (miny, maxy) = min_max m in
  minx -- maxx
  |> Iter.iter (fun i ->
      miny -- maxy
      |> Iter.iter (fun j ->
          match MC.get (i,j) m with
          | None -> pp_none fmt ()
          | Some v -> pp_v fmt v);
      CCFormat.fprintf fmt "@.")


let coord_neigh_c_i (x, y) =
  [
    (x + 1, y);
    (x - 1, y);
    (x, y + 1);
    (x, y - 1);
  ]

let coord_neigh_i (x, y) =
  [
    (x + 1, y);
    (x - 1, y);
    (x, y + 1);
    (x, y - 1);
    (x + 1, y + 1);
    (x + 1, y - 1);
    (x - 1, y + 1);
    (x - 1, y - 1);
  ]

let iter_neigh_i p m =
  Iter.of_list (coord_neigh_i p) |> Iter.filter (fun p -> MC.mem p m)

let iter_neigh p m =
  iter_neigh_i p m
  |> Iter.map (fun p ->
         MC.get p m |> CCOption.get_exn_or "impossible None, mem was checked"
         |> fun o -> (p, o))

let exists_neigh_i f p m = Iter.exists (fun (p,_) -> f p) (iter_neigh p m)
let exists_neigh f p m = Iter.exists (fun (_,e) -> f e) (iter_neigh p m)
