(* ==== Puzzle 09 : https://adventofcode.com/2022/day/9 ==== *)

module PosSet = Set.Make(struct
  type t = int * int
  let compare = compare
end)

type direction = Up | Down | Left | Right

type rope = int * int list

let add_offset (i, j) = function
  | Up -> (i+1, j)
  | Down -> (i-1, j)
  | Left -> (i, j-1)
  | Right -> (i, j+1)

let adjacent_hd_tl (xh, yh) (xt, yt) =
  abs (xh - xt) <= 1 && abs (yh - yt) <= 1

let sign a b =
  if a = b then 0
  else if a > b then 1
  else -1

let move_tail (xh, yh) (xt, yt) =
  let xt = xt + sign xh xt in
  let yt = yt + sign yh yt in
  (xt, yt)

let update_tail hd tl =
  if adjacent_hd_tl hd tl
  then tl
  else move_tail hd tl

let rec update_rope = function
  | [] -> failwith "empty"
  | t::[] -> t::[], t
  | hd::tl::rest ->
      let tl = update_tail hd tl in
      let new_rope, tail_pos = update_rope (tl::rest) in
      hd::new_rope, tail_pos

let move_hd dir = function
  | [] -> failwith "empty list"
  | t::q -> add_offset t dir :: q

let rec move n dir rope visited =
  match n with
  | 0 -> rope, visited
  | _ ->
      let rope = move_hd dir rope in
      let rope, tail = update_rope rope in
      move (n-1) dir rope (PosSet.add tail visited)


let string_suffix str start =
  String.sub str start (String.length str - start)

let parse_line line =
  let dir = match line.[0] with
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | c -> failwith "Wrong direction char"
  in dir, int_of_string (string_suffix line 2)

let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

let part1 () =
  let lines = read_all_lines [] in
  let _rope, visited = List.fold_left
    (fun (rope,visited) (dir,n) -> move n dir rope visited)
    ([(0,0); (0,0)], PosSet.singleton (0,0))
    lines
  in
  Format.printf "%d\n" (PosSet.cardinal visited)

(* let () = part1 () *)

let part2 () =
  let lines = read_all_lines [] in
  let _rope, visited = List.fold_left
    (fun (rope,visited) (dir,n) -> move n dir rope visited)
    (List.init 10 (fun _ -> 0,0), PosSet.singleton (0,0))
    lines
  in
  Format.printf "%d\n" (PosSet.cardinal visited)

let () = part2 ()
