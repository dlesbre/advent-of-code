(* ==== Puzzle 19 : https://adventofcode.com/2024/day/19 ====  *)

type color = White | Blue | Black | Red | Green

let color = function
  | 'w' -> White
  | 'u' -> Blue
  | 'b' -> Black
  | 'r' -> Red
  | 'g' -> Green
  | _ -> failwith "invalid color"

let color_id = function (* This is most likely compiled to no-op *)
  | White -> 0
  | Blue -> 1
  | Black -> 2
  | Red -> 3
  | Green -> 4

type color_trie = {
  children: color_trie option array;
  stop: bool;
}

let empty_trie () = {
  children = Array.make 5 None;
  stop = false;
}

let rec add_to_trie color_trie = function
  | [] -> { color_trie with stop=true }
  | c::cs ->
      let id = color_id c in
      color_trie.children.(id) <- Some (
        match color_trie.children.(id) with
        | Some x -> add_to_trie x cs
        | None -> add_to_trie (empty_trie ()) cs);
      color_trie
let parse_pattern pattern = String.fold_right (fun c l -> color c::l) pattern []

let preprocess = function
  | towels::""::patterns ->
          String.split_on_char ',' towels
          |> List.map String.trim
          |> List.map parse_pattern
          |> List.fold_left add_to_trie (empty_trie ()),
          List.map parse_pattern patterns
  | _ -> failwith "Invalid format"

let rec can_create table color_trie pattern =
  match Hashtbl.find_opt table pattern with
  | Some b -> b
  | None ->
  let rec aux trie pat =
    match pat with
    | [] -> trie.stop
    | c::cs -> match trie.children.(color_id c) with
              | None -> trie.stop && can_create table color_trie pat
              | Some child ->
                aux child cs || (trie.stop && can_create table color_trie pat)
  in let result = aux color_trie pattern in
  Hashtbl.add table pattern result;
  result


let part1 (color_trie, patterns) =
  let table = Hashtbl.create 4096 in
  list_count (can_create table color_trie) patterns


let rec count_arrangements table color_trie pattern =
  match Hashtbl.find_opt table pattern with
  | Some b -> b
  | None ->
  let rec aux trie pat =
    match pat with
    | [] -> if trie.stop then 1 else 0
    | c::cs ->
        match trie.children.(color_id c) with
        | None -> if trie.stop then count_arrangements table color_trie pat else 0
        | Some child -> aux child cs +
                 (if trie.stop then count_arrangements table color_trie pat else 0)
    in
  let result = aux color_trie pattern in
  Hashtbl.add table pattern result;
  result

let part2 (color_trie, patterns) =
  let table = Hashtbl.create 4096 in
  list_sum (count_arrangements table color_trie) patterns

let () = register_int ~year:2024 ~day:19 ~preprocess ~part1 ~part2
