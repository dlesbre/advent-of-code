(* ==== Puzzle 25 : https://adventofcode.com/2022/day/25 ==== *)

let snafu2deci =
  String.fold_left (fun n c ->
    5*n + match c with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '-' -> -1
    | '=' -> -2
    | _ -> failwith "Invalid SNAFU number"
  ) 0

let mod_5 x = (* modulo between -2 and 2*)
  match x mod 5 with
  | 4 -> -1
  | 3 -> -2
  | -3 -> 2
  | -4 -> 1
  | z -> z

let snafu_str = function
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | -1 -> "-"
  | -2 -> "="
  | _ -> failwith "Invalid SNAFU digit"

let rec deci2snafu x str =
  if x = 0
  then str
  else
    let c = mod_5 x in
    deci2snafu (x/5 + (if c < 0 then 1 else 0)) (snafu_str c^str)


let rec part1 acc =
  try
    let line = read_line () in
    let nb = snafu2deci line in
    part1 (nb + acc)
  with End_of_file ->
    Format.printf "%d so %s@." acc (deci2snafu acc "")

let () = part1 0
