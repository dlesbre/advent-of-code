(* ==== Puzzle 01 : https://adventofcode.com/2025/day/1 ====  *)

let initial_dial_position = 50
let dial_modulo = 100

let parse_line l =
  let offset = Utils.string_suffix l 1 |> int_of_string in
  match l.[0] with
  | 'L' -> -offset
  | 'R' -> offset
  | _ -> failwith "invalid input"
let preprocess = List.map parse_line
let part1 offsets =
  let folder (nb_zeros, pos) elt =
    let pos = (pos + elt) % dial_modulo in
    (nb_zeros + Bool.to_int (pos = 0), pos)
  in
  List.fold_left folder (0,initial_dial_position) offsets |> fst

let part2 offsets =
  let folder (nb_zeros, pos) elt =
    let npos = pos + elt in
    let count =
      if npos < 0 then - npos / dial_modulo + Bool.to_int(pos <> 0)
      else if npos = 0 then 1
      else npos / dial_modulo in
    nb_zeros + count, npos % dial_modulo
  in
  List.fold_left folder (0,initial_dial_position) offsets |> fst

let () = register_int ~year:2025 ~day:01 ~preprocess ~part1 ~part2
