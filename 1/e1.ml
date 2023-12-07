open Helpers

let test_input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
let if_none o e = match o with None -> Some e | Some _ -> o

let find_firstlast_number s =
  let rec aux i first last =
    match i with
    | -1 ->
        ( CCOption.get_exn_or "oopsy first" first,
          CCOption.get_exn_or "oopsy last" last )
    | _ -> (
        match s.[i] with
        | '0' .. '9' as n -> aux (i - 1) (Some n) (if_none last n)
        | _ -> aux (i - 1) first last)
  in
  let f, l = aux (CCString.length s - 1) None None in
  int_of_string (CCChar.to_string f ^ CCChar.to_string l)

let run input =
  input |> CCString.lines
  |> CCList.map find_firstlast_number
  |> CCList.fold_left ( + ) 0

let () =
  assert (run test_input == 142);

  let input = Lwt_main.run (Aoc.input 1) in
  print_int (run input)
