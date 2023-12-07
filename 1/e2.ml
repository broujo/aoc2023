let test_input =
  "two1nine\n\
   eightwothree\n\
   abcone2threexyz\n\
   xtwone3four\n\
   4nineeightseven2\n\
   zoneight234\n\
   7pqrstsixteen"

let if_none o e = match o with None -> Some e | Some _ -> o
let if_none_f2 f t r o = match o with Some _ -> o | None -> f t r

let check_wraped s i t r =
  CCOption.wrap
    (fun s ->
      if CCString.sub s i (CCString.length t) = t then r else failwith "nope")
    s

let spelled s i =
  let f = check_wraped s i in
  None |> if_none_f2 f "one" '1' |> if_none_f2 f "two" '2'
  |> if_none_f2 f "three" '3' |> if_none_f2 f "four" '4'
  |> if_none_f2 f "five" '5' |> if_none_f2 f "six" '6'
  |> if_none_f2 f "seven" '7' |> if_none_f2 f "eight" '8'
  |> if_none_f2 f "nine" '9'

let find_firstlast_number s =
  let rec aux i first last =
    match i with
    | -1 ->
        ( CCOption.get_exn_or "oopsy first" first,
          CCOption.get_exn_or "oopsy last" last )
    | _ -> (
        match s.[i] with
        | '0' .. '9' as n -> aux (i - 1) (Some n) (if_none last n)
        | _ -> (
            let sp = spelled s i in
            match sp with
            | None -> aux (i - 1) first last
            | Some n -> aux (i - 1) (Some n) (if_none last n)))
  in
  let f, l = aux (CCString.length s - 1) None None in
  int_of_string (CCChar.to_string f ^ CCChar.to_string l)

let run input =
  input |> CCString.lines
  |> CCList.map find_firstlast_number
  |> CCList.fold_left ( + ) 0

let () =
  assert (run test_input == 281);

  let input = Lwt_main.run (Helpers.Aoc.input 1) in
  print_int (run input)

