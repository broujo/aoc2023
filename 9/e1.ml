open Helpers
open Parser

let test_input = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

let diff_map l =
  let s = CCList.rev l |> CCList.to_seq in
  let rec aux acc s =
    if CCSeq.for_all (CCInt.equal 0) s then acc else
    let s' = CCSeq.map2 (-) s (CCSeq.tail_exn s) in
    aux (CCSeq.cons s' acc) s'
  in aux (CCSeq.return s) s

let get_next smap =
  CCSeq.map (CCSeq.head_exn) smap
  |> CCSeq.fold (+) 0
  
let run input =
  input |> CCString.lines |> CCList.map parse
  |> CCList.map diff_map
  |> CCList.map get_next
  |> CCList.fold_left (+) 0

let () =
  assert (run test_input = 114);

  let input = Lwt_main.run (Aoc.input 9) in
  print_int (run input)
