(* ==== Puzzle 25 : https://adventofcode.com/2024/day/25 ====  *)

let parse_lock_or_key (lock, keys) x =
  if List.hd x = "#####" then (* lock *)
    Array.init 5 (fun i ->
      match list_find_first (fun c -> c.[i] = '.') x with
      | Some (_,n) -> n - 1
      | _ -> failwith "Invalid lock format"
      )::lock, keys
  else (* key *)
    lock, Array.init 5 (fun i ->
      match list_find_first (fun c -> c.[i] = '#') x with
      | Some (_,n) -> 6 - n
      | _ -> failwith "Invalid key format"
      )::keys

let preprocess input =
  list_split (fun x -> x = "") input |>
  List.fold_left parse_lock_or_key ([], [])

let rec fit lock key = function
  | -1 -> true
  | n -> lock.(n) + key.(n) < 6 && fit lock key (n-1)
let fit lock key = fit lock key 4

let part1 (locks, keys) =
  list_sum (fun lock -> list_count (fun key -> fit lock key) keys) locks

let part2 _ = 0

let () = register_int ~year:2024 ~day:25 ~preprocess ~part1 ~part2
