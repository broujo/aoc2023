open Angstrom
open Helpers.Parsing

let p_time = string "Time:" *> skip_space1 *> p_int_list
let p_distance = string "Distance:" *> skip_space1 *> p_int_list
let p_all = lift2 CCList.combine (p_time <* skip_nl1) (p_distance <* skip_nl)

let parse (input : string) =
  CCResult.get_or_failwith (parse_string ~consume:All p_all input)
