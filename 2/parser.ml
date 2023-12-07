open Angstrom
open Helpers.Parsing

(* Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green *)
type color = Blue | Green | Red
type drawc = color * int
type draw = { red : int; blue : int; green : int }
type game = { n : int; draws : draw list }

let p_blue = string "blue" *> return Blue
let p_red = string "red" *> return Red
let p_green = string "green" *> return Green
let p_color = choice ~failure_msg:"Bad color" [ p_red; p_blue; p_green ]
let p_drawc = lift2 (fun c n -> (n, c)) p_integer (string " " *> p_color)
let p_drawc_list = sep_by (string ", ") p_drawc

let drawc_to_draw l =
  let rec aux res l =
    match l with
    | [] -> res
    | (Blue, n) :: tl -> aux { res with blue = res.blue + n } tl
    | (Red, n) :: tl -> aux { res with red = res.red + n } tl
    | (Green, n) :: tl -> aux { res with green = res.green + n } tl
  in
  aux { blue = 0; red = 0; green = 0 } l

let p_draw = lift drawc_to_draw p_drawc_list
let p_draws = sep_by (string "; ") p_draw

let p_game =
  lift2
    (fun n l -> { n; draws = l })
    (string "Game " *> p_integer <* string ": ")
    p_draws

let parse input =
  CCResult.get_or_failwith (parse_string ~consume:All p_game input)
