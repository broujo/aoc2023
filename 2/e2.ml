open Helpers
open Parser

let test_input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

let maxx game =
  let rec aux mg draws =
    match draws with
    | [] -> mg
    | {blue; green; red} :: q -> aux {blue = max mg.blue blue; red = max mg.red red; green = max mg.green green } q 
  in aux {blue = 0; red = 0; green = 0} game.draws

let power game =
  game.blue * game.red * game.green

let is_possible game =
  let rec aux draws =
    match draws with
    | [] -> true
    | {blue; green; red} :: q -> blue <= 14 && green <= 13 && red <= 12 && aux q
  in aux game.draws

let value game =
  if is_possible game then game.n else 0

let run input =
  input |> CCString.lines
  |> CCList.map parse
  |> CCList.map maxx
  |> CCList.map power
  |> CCList.fold_left (+) 0 

let () =
  assert (run test_input == 2286);

  let input = Lwt_main.run (Aoc.input 2) in
  print_int (run input)
