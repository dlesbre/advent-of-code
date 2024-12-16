(* ==== Puzzle 07 : https://adventofcode.com/2024/day/7 ====  *)

type equation = {
  total: int;
  numbers: int list
}

let parse_line line =
  match String.split_on_char ':' line with
    | [before; after] ->
      { total = int_of_string before;
        numbers =
          String.sub after 1 (String.length after - 1) |>
          String.split_on_char ' ' |>
          List.map int_of_string
      }
    | _ -> failwith "Invalid format"

let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev_map parse_line acc

let rec solvable total partial_total = function
  | [] -> total = partial_total
  | n::ns ->
      let using_plus = partial_total + n in
      (using_plus <= total && solvable total using_plus ns)
      ||
      let using_prod = partial_total * n in
      (using_prod <= total && solvable total using_prod ns)

let rec cat a b n =  if b < n then a*n + b else cat a b (10*n)

let rec solvable_with_cat total partial_total = function
  | [] -> total = partial_total
  | n::ns ->
      let using_plus = partial_total + n in
      (using_plus <= total && solvable_with_cat total using_plus ns)
      ||
      let using_prod = partial_total * n in
      (using_prod <= total && solvable_with_cat total using_prod ns)
      ||
      let using_cat = cat partial_total n 10 in
      (using_cat <= total && solvable_with_cat total using_cat ns)


let preprocess input = List.map parse_line input

let part1 input = List.fold_left (fun total eq ->
    if solvable eq.total (List.hd eq.numbers) (List.tl eq.numbers)
    then total + eq.total
    else total
  ) 0 input
let part2 input = List.fold_left (fun total eq ->
  if solvable_with_cat eq.total (List.hd eq.numbers) (List.tl eq.numbers)
  then total + eq.total
  else total
) 0 input

let () = register_int ~year:2024 ~day:07 ~preprocess ~part1 ~part2
