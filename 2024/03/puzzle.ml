(* ==== Puzzle 03 : https://adventofcode.com/2024/day/3 ====  *)

let read_input () =
  let rec aux acc = match read_line () with
  | s -> aux (acc ^ s)
  | exception End_of_file -> acc
  in aux ""

let mul = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|}

let part1 string =
  let matches = Str.full_split mul string in
  List.fold_left (fun acc v -> match v with
   | Str.Text s -> acc
   | Str.Delim d -> acc + Scanf.sscanf d "mul(%d,%d)" ( * )
  ) 0 matches

let mul_do_dont = Str.regexp {|mul([0-9]+,[0-9]+)\|do()\|don't()|}

let part2 string =
  let matches = Str.full_split mul_do_dont string in
  fst @@ List.fold_left (fun ((nb, enabled) as acc) v -> match v with
    | Str.Text s -> acc
    | Str.Delim d ->
      match d with
      | "do()" -> (nb, true)
      | "don't()" -> (nb, false)
      | _ ->
          if enabled
          then (nb + Scanf.sscanf d "mul(%d,%d)" ( * ), true  )
          else acc
  ) (0, true) matches

let main () =
  let reports = read_input () in
  Format.printf "Part 1 : %d@." (part1 reports);
  Format.printf "Part 2 : %d@." (part2 reports)

let () = main ()
