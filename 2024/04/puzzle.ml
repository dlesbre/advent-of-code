(* ==== Puzzle 04 : https://adventofcode.com/2024/day/4 ====  *)

let rec read_all_lines acc =
  try
    read_all_lines (read_line ()::acc)
  with End_of_file ->
    List.rev acc

let parse_line line =
  Array.init (String.length line) (fun i -> line.[i])

let parse_grid lines =
  let lines = List.map parse_line lines in
  Array.of_list lines



module Direction = struct
  type t = N | NE | E | SE | S | SW | W | NW

  let all_directions = [N; NE; E; SE; S; SW; W; NW]

  let fold f init =
    List.fold_left f init all_directions

  let offset = function
    | N ->  (-1, 0)
    | NE -> (-1, 1)
    | E ->  ( 0, 1)
    | SE -> ( 1, 1)
    | S ->  ( 1, 0)
    | SW -> ( 1,-1)
    | W ->  ( 0,-1)
    | NW -> (-1,-1)

  (* let array_offset_access array direction y x =
    let (diff_y, diff_x) = offset direction in *)
end

let grid_foldi (f : int -> int -> 'a -> 'acc -> 'acc) (init: 'acc) (a : 'a array array) =
  Array.fold_left (fun (acc, line_nb) line ->
    Array.fold_left (fun s elt ->
      let (acc, col_nb) = s in
      f line_nb col_nb elt acc, col_nb+1)
    (acc, 0) line |> fst, line_nb+1)
  (init, 0) a
  |> fst

let array_get_opt array x =
  let n = Array.length array in
  if 0 <= x && x < n
  then Some (Array.unsafe_get array x)
  else None

let grid_get grid (x,y) =
  match array_get_opt grid y with
  | None -> None
  | Some line -> array_get_opt line x

let grid_unsafe_get grid (x,y) = Array.unsafe_get (Array.unsafe_get grid y) x

let ( *| ) n (x,y) = (n*x, n*y)

let ( +| ) (x,y) (x', y') = (x+x', y+y')

let part1 grid =
  grid_foldi (fun line col elt total ->
    if Char.equal elt 'X' then
    let root = (col, line) in
    Direction.fold (fun total dir ->
      let offset = Direction.offset dir in
      match grid_get grid (3 *| offset +| root) with
      | Some 'S' ->
          if Char.equal (grid_unsafe_get grid (offset +| root)) 'M'
             && Char.equal (grid_unsafe_get grid (2 *| offset +| root)) 'A'
                then total + 1
                 else total
      | _ -> total
      ) total
    else total
    ) 0 grid

let (let*) = Option.bind

let diag1 = Direction.offset Direction.NE
let diag1' = Direction.offset Direction.SW
let diag2 = Direction.offset Direction.SE
let diag2' = Direction.offset Direction.NE

let is_m_or_s c = c = 'M' || c = 'S'

let check_diag grid root diag =
  match
    let* c = grid_get grid (root +| diag) in
    if is_m_or_s c then
      let* c' = grid_get grid (root +| (-1 *| diag)) in
      if c' != c && is_m_or_s c' then Some () else None
    else None
  with
    | None -> 0
    | Some () -> 1

let part2 grid =
  grid_foldi (fun line col elt total ->
    if Char.equal elt 'A' then
      let root = (col, line) in
      total + (check_diag grid root diag1) * (check_diag grid root diag2)
    else total
    ) 0 grid

let main () =
  let grid = parse_grid (read_all_lines []) in
  Format.printf "Part 1 : %d@." (part1 grid);
  Format.printf "Part 2 : %d@." (part2 grid)

let () = main ()
