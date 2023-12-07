open Angstrom
open Helpers.Parsing

(* Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53 *)
type card = { n : int; card : int list; owned : int list }

let skip_space1 = skip_many1 (char ' ')

let p_card_n = string "Card" *> skip_space1 *> p_integer <* char ':' <* skip_space1 <?> "parser card"
let p_int_list = sep_by1 skip_space1 p_integer <?> "parser int list"

let p_card =
  lift3
    (fun n card owned -> { n; card; owned })
    p_card_n
    (p_int_list <* skip_space1 <* string "|" <* skip_space1)
    p_int_list

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:All p_card input)
