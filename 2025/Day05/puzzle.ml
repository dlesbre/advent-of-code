(* ==== Puzzle 05 : https://adventofcode.com/2025/day/5 ====  *)

let preprocess input =
  match list_split (fun l -> l = "") input with
  | [ranges; ids] ->
      let ranges = List.map (fun x -> Scanf.sscanf x "%d-%d" (fun lo hi -> (lo,hi))) ranges in
      let ids = List.map int_of_string ids in
      (ranges, ids)
  | _ -> failwith "invalid input"

let in_range id (lo, hi) = lo <= id && id <= hi
let is_fresh ranges id = List.exists (in_range id) ranges
let part1 (ranges, ids) = list_count (is_fresh ranges) ids

let overlaps ((lo1, hi1) as r1) ((lo2, hi2) as r2) =
  in_range lo2 r1 || in_range hi2 r1 || in_range lo1 r2 || in_range hi1 r2

let union (lo1, hi1) (lo2, hi2) = min lo1 lo2, max hi1 hi2


let rec insert_with_overlap range = function
  | [] -> [range], false
  | r::rs when overlaps r range -> union r range::rs, true
  | r::rs -> let rs, has_overlapped = insert_with_overlap range rs in
             r::rs, has_overlapped

let rec merge_overlaps ranges =
  let ranges, has_overlapped =
    List.fold_left (fun (new_list, has_overlapped) range ->
      let new_list, has_overlapped' = insert_with_overlap range new_list in
      new_list, has_overlapped || has_overlapped') ([], false) ranges
  in if has_overlapped then merge_overlaps ranges else ranges

let part2 (ranges, _) =
  merge_overlaps ranges
  |> list_sum (fun (lo, hi) -> hi - lo + 1)


let () = register_int ~year:2025 ~day:05 ~preprocess ~part1 ~part2
