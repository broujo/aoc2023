open Angstrom
open Helpers.Parsing

let p_all = sep_by1 skip_space1 p_integer

let parse (input : string) =
  CCResult.get_or_failwith (parse_string ~consume:All p_all input)
