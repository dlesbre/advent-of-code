(* ==== Puzzle 03 : https://adventofcode.com/2024/day/3 ====  *)

let preprocess input = String.concat "" input

let mul = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}

let part1 string =
  let matches = Str.full_split mul string in
  List.fold_left (fun acc v -> match v with
   | Str.Text _ -> acc
   | Str.Delim d -> acc + Scanf.sscanf d "mul(%d,%d)" ( * )
  ) 0 matches

let mul_do_dont = Str.regexp {|mul([0-9]+,[0-9]+)\|do()\|don't()|}

let part2 string =
  let matches = Str.full_split mul_do_dont string in
  fst @@ List.fold_left (fun ((nb, enabled) as acc) v -> match v with
    | Str.Text _ -> acc
    | Str.Delim d ->
      match d with
      | "do()" -> (nb, true)
      | "don't()" -> (nb, false)
      | _ ->
          if enabled
          then (nb + Scanf.sscanf d "mul(%d,%d)" ( * ), true  )
          else acc
  ) (0, true) matches


let () = register_int ~year:2024 ~day:03 ~preprocess ~part1 ~part2
