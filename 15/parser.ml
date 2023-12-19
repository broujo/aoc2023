open Angstrom
open Helpers.Parsing

type operation = Dash | Equal of int

let p_label = take_while (function ',' | '=' | '-' -> false | _ -> true)

let p_dash =
  lift (function _ -> Dash) (char '-')

let p_equal =
  lift (function i -> Equal i) (char '=' *> p_integer)

let p_all =
  both p_label (p_dash <|> p_equal)

let parse (input : string) =
  CCResult.get_or_failwith (parse_string ~consume:All p_all input)
