open Angstrom
open Helpers.Parsing

type direction = R | L
type node = { left : string; right : string }

let node left right = { left; right }

module Graph = CCMap.Make (CCString)

let direction c = match c with 'R' -> R | 'L' -> L | _ -> failwith "nope"

let p_direction =
  lift direction (satisfy (function 'R' | 'L' -> true | _ -> false))

let p_instruction =
  many_till p_direction (skip_nl1)

let p_name = take 3

let p_node_dir =
  lift2 node (char '(' *> p_name <* string ", ") (p_name <* char ')')
  <?> "node dir"

let p_node = both (p_name <* string " = ") p_node_dir <?> "node"
let gr (k, v) g = Graph.add k v g

let p_network =
  fix (fun m -> lift2 gr p_node (char '\n' *> m <|> return Graph.empty)) <?> "network"

let p_all = both p_instruction p_network <?> "all" <* skip_nl

let parse (input : string) =
  CCResult.get_or_failwith (parse_string ~consume:All p_all input)
