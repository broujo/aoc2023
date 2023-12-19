open Helpers

let test_input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let hash_step current c =
  ((current + CCChar.to_int c) * 17) mod 256

let hash s =
  CCString.fold hash_step 0 s

let run input =
  input |> CCString.lines |> CCList.map (CCString.split_on_char ',')
  |> CCList.flatten
  |> CCList.map hash
  |> CCList.fold_left (+) 0

let () =
  assert (run test_input = 1320);

  let input = Lwt_main.run (Aoc.input 15) in
  print_int (run input)
