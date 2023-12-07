open Angstrom

let p_integer =
  take_while1 (function '0' .. '9' -> true | '-' -> true | _ -> false)
  >>| int_of_string

let skip_space1 = skip_many1 (char ' ')
let skip_nl1 = skip_many1 (char '\n')
let skip_nl = skip_many (char '\n')
let p_int_list = sep_by1 skip_space1 p_integer <?> "parser int list"
