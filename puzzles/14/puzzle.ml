(* ==== Puzzle 14 : https://adventofcode.com/2022/day/14 ==== *)

let parse_pair line =
  let line = (* we split on the ">" of "->" so we need to remove the "-" *)
    if String.ends_with ~suffix:"-" line
    then String.sub line 0 (String.length line - 1)
    else line
  in
  match List.map (fun x -> int_of_string (String.trim x)) (String.split_on_char ',' line) with
  | [x;y] -> x, y
  | _ -> failwith "Incorrect format"

let parse_line line =
  List.map parse_pair (String.split_on_char '>' line)

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

let source_x = 500
let source_y = 0

let rocks = read_all_lines []

let max_x, min_x, max_y, min_y =
  List.fold_left (fun (max_x, min_x, max_y, min_y) (x,y) ->
    let max_x = max max_x x in
    let min_x = min min_x x in
    let max_y = max max_y y in
    let min_y = min min_y y in
    max_x, min_x, max_y, min_y) (source_x,source_x,source_y,source_y) (List.flatten rocks)

(* for part 2 - increase y and x *)
let max_y = max_y + 2
let height = max_y - min_y + 1

(* must be wide enough for full pyramid *)
let max_x = max max_x (source_x + height + 1)
let min_x = min min_x (source_x - height - 1)

let width = max_x - min_x + 1


type space = Air | Rock | Sand

let map = Array.init width (fun _ -> Array.make height Air)

let sort a b = if a < b then a,b else b,a

let fill_path (x1,y1) (x2,y2) =
  let x1, x2 = sort x1 x2 in
  let y1, y2 = sort y1 y2 in
  for x = x1 to x2 do
    for y = y1 to y2 do
      map.(x - min_x).(y - min_y) <- Rock
    done
  done

let rec make_path = function
  | t::q::r -> fill_path t q; make_path (q::r)
  | _ -> ()

exception Out_of_bounds

let can_move x y =
  if (min_x <= x && x <= max_x && min_y <= y && y <= max_y) then
    map.(x - min_x).(y - min_y) = Air
  else raise Out_of_bounds

let rec propagate_sand x y =
  let y' = y+1 in
  if can_move x y' then propagate_sand x y'
  else if can_move (x-1) y' then propagate_sand (x-1) y'
  else if can_move (x+1) y' then propagate_sand (x+1) y'
  else (map.(x - min_x).(y - min_y) <- Sand; x, y)

let rec count_propagations n =
  (* Format.printf "Counting %d\n" n; *)
  let ok =
  try
    let x, y = propagate_sand source_x source_y in
    if x = source_x && y = source_y then false else
    true
  with Out_of_bounds -> false in
  if ok then count_propagations (n+1) else n


let part1 () =
  List.iter make_path rocks;
  let x = count_propagations 0 in
  Format.printf "%d\n" x

let part2 () =
  (* add floor to list of paths *)
  List.iter make_path ([(min_x, max_y); (max_x, max_y)]::rocks);
  let x = count_propagations 0 + 1 in (* doesn't count the last unit *)
  Format.printf "%d\n" x

let () = part2 ()
