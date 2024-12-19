(* ==== Puzzle 19 : https://adventofcode.com/2024/day/19 ====  *)

type color = White | Blue | Black | Red | Green

let color = function
  | 'w' -> White
  | 'u' -> Blue
  | 'b' -> Black
  | 'r' -> Red
  | 'g' -> Green
  | _ -> failwith "invalid color"

let color_id = function
  | White -> 0
  | Blue -> 1
  | Black -> 2
  | Red -> 3
  | Green -> 4

type color_trie = {
  children: color_trie option array;
  stop: bool;
  id: int; (* unique id for hashing *)
}
let counter = ref 0
let empty_trie () = incr counter; {
  children = Array.make 5 None;
  stop = false;
  id = !counter;
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

let string_strip x =
  if String.starts_with ~prefix:" " x then
    String.sub x 1 (String.length x-1)
  else x

let preprocess = function
  | towels::""::patterns ->
          String.split_on_char ',' towels
          |> List.map string_strip
          |> List.map parse_pattern
          |> List.fold_left add_to_trie (empty_trie ()),
          List.map parse_pattern patterns
  | _ -> failwith "Invalid format"

module H = Hashtbl.Make(struct
  type t = color_trie * color list
  let hash (x,y) = Hashtbl.hash (x.id,y)
  let equal (x,y) (x',y') = x.id = x'.id && y = y'
end)

let can_create table color_trie pattern =
  let rec aux trie pat b =
    match H.find_opt table (trie, pat) with
    | Some b -> b
    | None ->
    match pat with
    | [] -> trie.stop
    | c::cs ->
        let result =
        match trie.children.(color_id c) with
        | None -> trie.stop && b && aux color_trie pat false
        | Some child ->
          aux child cs true || (trie.stop && b && aux color_trie pat false)
        in
        H.add table (trie, pat) result;
        result
    in aux color_trie pattern false


let part1 (color_trie, patterns) =
  let table = H.create 4096 in
  List.fold_left (fun total pat -> if can_create table color_trie pat then total+1 else total) 0 patterns


let count_arrangements table color_trie pattern =
  let rec aux trie pat b =
    match H.find_opt table (trie, pat) with
    | Some b -> b
    | None ->
    match pat with
    | [] -> if trie.stop then 1 else 0
    | c::cs ->
        let result =
        match trie.children.(color_id c) with
        | None -> if trie.stop && b then aux color_trie pat false else 0
        | Some child ->
          aux child cs true + (if trie.stop && b then aux color_trie pat false else 0)
        in
        H.add table (trie, pat) result;
        result
    in aux color_trie pattern false

let part2 (color_trie, patterns) =
  let table = H.create 4096 in
  List.fold_left (fun total pat -> total + count_arrangements table color_trie pat) 0 patterns

let () = register_int ~year:2024 ~day:19 ~preprocess ~part1 ~part2
