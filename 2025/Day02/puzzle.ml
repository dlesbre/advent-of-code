(* ==== Puzzle 02 : https://adventofcode.com/2025/day/2 ====  *)

let parse_range str = Scanf.sscanf str "%d-%d" (fun start stop -> start, stop)

module Decimal = struct
  type t = int array
  let of_int =
    let rec runner size digits x =
      if x = 0 then Array.of_list digits
      else runner (size+1) ((x mod 10)::digits) (x/10)
    in runner 0 []

  let to_int = Array.fold_left (fun acc i -> acc * 10 + i) 0
end

let preprocess = function
  | [line] ->
      String.split_on_char ',' line |>
      List.map parse_range
  | _ -> failwith "Invalid multiline input"

let rec next_invalid i step =
  let repr = Decimal.of_int i in
  if Array.length repr mod step <> 0 (* Not the right length for current step - try longer *)
  then next_invalid (pow 10 (Array.length repr)) step
  else
    let pattern_len = Array.length repr / step in
    Range.iter (fun i ->
      Range.iter (fun j ->
        repr.(i + j * pattern_len) <- repr.(i))
      (Range.interval 1 step))
    (Range.upto pattern_len);
    let candidate = Decimal.to_int repr in
    if candidate >= i then candidate else
      let step_nb = pow 10 ((step - 1) * pattern_len) in
      next_invalid ((i+step_nb) / step_nb * step_nb) step

let rec all_invalids set start stop step =
  let i = next_invalid start step in
  if i <= stop then all_invalids (IntSet.add i set) (i+1) stop step else set

let part1 = list_sum (fun (i,j) ->
  let invalids = all_invalids IntSet.empty i j 2 in
  IntSet.fold (+) invalids 0)

let part2 = list_sum (fun (i,j) ->
  let step = Decimal.of_int j |> Array.length in
  let invalids = Range.fold (fun step acc -> all_invalids acc i j step) (Range.interval 2 (step+1)) IntSet.empty in
  IntSet.fold (+) invalids 0)

let () = register_int ~year:2025 ~day:02 ~preprocess ~part1 ~part2
