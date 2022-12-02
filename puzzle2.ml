type shape = Rock | Paper | Scissors
type result = Win | Loss | Draw

let shape_score = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
(*
let shape_of_int i =
  let i = i mod 3 in
  let i = if i < 0 then i + 3 else i in
  match i with
  | 0 -> Scissors
  | 1 -> Rock
  | 2 -> Paper
  | _ -> assert false *)

let result_score = function
  | Win -> 6
  | Draw -> 3
  | Loss -> 0

let result opponent_play my_play = match opponent_play, my_play with
  | Rock, Paper
  | Paper, Scissors
  | Scissors, Rock -> Win
  | Rock, Scissors
  | Paper, Rock
  | Scissors, Paper -> Loss
  | Rock, Rock
  | Paper, Paper
  | Scissors, Scissors -> Draw

let round_score opponent_play my_play =
  shape_score my_play + result_score (result opponent_play my_play)

let decipher_abc = function
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissors
  | _ -> failwith "Unknown char (not A B or C)"

let decipher_xyz = function
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Scissors
  | _ -> failwith "Unknown char (not X Y or Z)"

let rec inputs found =
  try
    let line = read_line () in
    inputs ((line.[0], line.[2])::found)
  with End_of_file -> found

let get_scores () =
  let inputs = inputs [] in
  let plays = List.map (fun (opp,me) -> decipher_abc opp, decipher_xyz me) inputs in
  let scores = List.map (fun (opp,me) -> round_score opp me) plays in
  List.fold_left (+) 0 scores

(* let () =
  let score = get_scores () in
  Format.printf "%d\n" score *)

(* ==== Part 2 ==== *)
let decipher_xyz = function
  | 'X' -> Loss
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith "Unknown char (not X Y or Z)"

let my_play opponent_play = function
  | Draw -> opponent_play
  | Win -> begin match opponent_play with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock
    end
  | Loss -> begin match opponent_play with
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper
    end

let get_scores2 () =
  let inputs = inputs [] in
  let plays = List.map (fun (opp,me) -> decipher_abc opp, decipher_xyz me) inputs in
  let scores = List.map (fun (opp,me) -> round_score opp (my_play opp me)) plays in
  List.fold_left (+) 0 scores

let () =
  let score = get_scores2 () in
  Format.printf "%d\n" score
