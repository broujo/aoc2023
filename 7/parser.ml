open Angstrom
open Helpers.Parsing

type card = C of char

let card c = C c
let p_card = lift card any_char
let p_hand = count 5 p_card
let p_bid = p_integer
let p_all = both p_hand (skip_space1 *> p_bid)

let parse (input : string) =
  CCResult.get_or_failwith (parse_string ~consume:All p_all input)
