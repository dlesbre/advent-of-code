(* ==== Puzzle 03 : https://adventofcode.com/2025/day/03 ====  *)

let preprocess input = input

let max_digit i char = function
  | None -> Some (i, char)
  | Some(_, char') when char > char' -> Some (i, char)
  | acc -> acc


let rec max_jolt acc start nb_remain bank =
  let stop = if nb_remain = 1 then None else Some (1-nb_remain) in
  match string_foldi max_digit (string_slice ~start ?stop bank) None with
  | None -> failwith "line too short"
  | Some(pos, digit) ->
      let acc = 10 * acc + parse_char digit in
      if nb_remain = 1 then acc
      else max_jolt acc (start + pos + 1) (nb_remain - 1) bank

let part1 = list_sum (max_jolt 0 0 2)
let part2 = list_sum (max_jolt 0 0 12)

let () = register_int ~year:2025 ~day:03 ~preprocess ~part1 ~part2
