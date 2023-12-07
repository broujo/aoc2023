open Helpers
open Parser

let test_input =
  "seeds: 79 14 55 13\n\n\
   seed-to-soil map:\n\
   50 98 2\n\
   52 50 48\n\n\
   soil-to-fertilizer map:\n\
   0 15 37\n\
   37 52 2\n\
   39 0 15\n\n\
   fertilizer-to-water map:\n\
   49 53 8\n\
   0 11 42\n\
   42 0 7\n\
   57 7 4\n\n\
   water-to-light map:\n\
   88 18 7\n\
   18 25 70\n\n\
   light-to-temperature map:\n\
   45 77 23\n\
   81 45 19\n\
   68 64 13\n\n\
   temperature-to-humidity map:\n\
   0 69 1\n\
   1 0 69\n\n\
   humidity-to-location map:\n\
   60 56 37\n\
   56 93 4"

let do_range n r =
  if n >= r.source && n < (r.source + r.length)
  then
    (n - r.source + r.dest), `Stop
  else
    n, `Continue

let apply_map n m =
  (* CCFormat.printf "%s: %d@ " m.from n; *)
  CCList.fold_while do_range n m.ranges

let apply_maps ms n =
  CCList.fold_left apply_map n ms

let do_something _ _ = ()

let run input =
  let seeds, maps = parse input in
  CCList.map (apply_maps maps) seeds
  |> CCList.fold_left min CCInt.max_int

let () =
  assert (run test_input == 35);

  let input = Lwt_main.run (Aoc.input 5) in
  print_int (run input)
