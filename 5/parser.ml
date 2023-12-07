open Angstrom
open Helpers.Parsing

type card = { n : int; card : int list; owned : int list }
type seed = int
type seeds = int list
type category = string
type range = { dest : int; source : int; length : int }
type map = { from : category; toc : category; ranges : range list }

let skip_space1 = skip_many1 (char ' ')
let skip_nl1 = skip_many1 (char '\n')
let skip_nl = skip_many (char '\n')

let p_int_list = sep_by1 skip_space1 p_integer <?> "parser int list"

let p_seeds = string "seeds:" *> skip_space1 *> p_int_list <?> "seeds"
let p_category = take_while (fun c -> c <> '-' && c <> ' ') <?> "category"

let p_range =
  lift3 (fun dest source length -> {dest; source; length})
  (p_integer <* skip_space1)
  (p_integer <* skip_space1)
  p_integer
  <?> "range"

let p_map =
  lift3 (fun from toc ranges -> {from; toc; ranges})
  (p_category <* string "-to-")
  (p_category <* char ' ' <* string "map:" <* skip_nl1)
  (sep_by1 (char '\n') p_range)
  <?> "map"

let p_maps =
  sep_by1 skip_nl1 p_map
  <?> "maps"

let p_all =
  lift2 (fun a b -> a, b)
  (p_seeds <* skip_nl1)
  (p_maps <* skip_nl)
  <?> "all"

let parse (input:string) =
  CCResult.get_or_failwith (parse_string ~consume:All p_all input)
