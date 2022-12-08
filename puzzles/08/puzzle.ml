(* ==== Puzzle 08 : https://adventofcode.com/2022/day/8 ==== *)

let rec read_all_lines acc =
  try
    read_all_lines (read_line ()::acc)
  with End_of_file ->
    List.rev acc

let parse_line line =
  Array.init (String.length line) (fun i -> int_of_char line.[i])

let parse_grid lines =
  let lines = List.map parse_line lines in
  Array.of_list lines

let grid = parse_grid (read_all_lines [])
let height = Array.length grid
let width = Array.length grid.(0)

type direction = Up | Down | Left | Right

let add_offset i j = function
  | Up -> (i-1, j)
  | Down -> (i+1, j)
  | Left -> (i, j-1)
  | Right -> (i, j+1)

let rec check_visible i j ht dir =
  let (i,j) = add_offset i j dir in
  if i < 0 || i >= height || j < 0 || j >= width
  then true
  else grid.(i).(j) < ht && (check_visible i j ht dir)

let check_visible_any i j =
  let ht = grid.(i).(j) in
  let vis = check_visible i j ht in
  vis Up || vis Down || vis Left || vis Right

let part1 () =
  let visible_trees =
    Array.init height (fun line ->
      Array.init width (fun col -> check_visible_any line col)
    ) in

  let count_visible =
    Array.fold_left (fun nb line ->
      Array.fold_left (fun nb a -> if a then nb+1 else nb) nb line
    ) 0 visible_trees in

  Format.printf "%d\n" count_visible

(* ==== part 2 ==== *)

let rec view_distance i j ht dir d =
  let (i,j) = add_offset i j dir in
  if i < 0 || i >= height || j < 0 || j >= width
  then d
  else
    if grid.(i).(j) < ht
    then view_distance i j ht dir (d+1)
    else d+1

let view_score i j =
  let ht = grid.(i).(j) in
  let vis = fun d -> view_distance i j ht d 0 in
  vis Up * vis Down * vis Left * vis Right

let part2 () =
  let maxi = ref min_int in
  Array.iteri (fun i line ->
    Array.iteri (fun j _ ->
      let score = view_score i j in
      maxi := max score !maxi
    ) line
  ) grid;
  Format.printf "%d\n" !maxi

let () = part2 ()
