(* ==== Puzzle 06 : https://adventofcode.com/2024/day/0  | None -> Some positions6 ====  *)

type direction = N | E | S | W

(** Set of direction, using a integer bits, saves ~0.17s compared to Set.Make *)
module DirectionSet : sig
  type t
  val empty : t
  val singleton : direction -> t
  val mem : direction -> t -> bool
  val add : direction -> t -> t
end = struct
  type t = int
  let empty = 0
  let singleton = function
    | N -> 1
    | E -> 2
    | S -> 4
    | W -> 8
  let add x s = singleton x lor s
  let mem x s = singleton x land s <> 0
end

type tile = Empty | Full | Guard of DirectionSet.t

let rec read_all_lines acc =
  try
    read_all_lines (read_line ()::acc)
  with End_of_file ->
    List.rev acc

let startpos = ref None

let parse_line line_nb line =
  Array.init (String.length line) (fun col_nb -> match line.[col_nb] with
  | '.' -> Empty
  | '#' -> Full
  | '^' -> startpos := Some(col_nb,line_nb); Guard (DirectionSet.singleton N)
  | _ -> failwith "Invalid input")

let parse_grid lines =
  let lines = List.mapi parse_line lines in
  Array.of_list lines

let offset = function
  | N ->  ( 0,-1)
  | E ->  ( 1, 0)
  | S ->  ( 0, 1)
  | W ->  (-1, 0)

let rotate_right = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N

let pp fmt = function
  | N -> Format.fprintf fmt "N"
  | E -> Format.fprintf fmt "E"
  | S -> Format.fprintf fmt "S"
  | W -> Format.fprintf fmt "W"

let ( +| ) (x,y) (x', y') = (x+x', y+y')

exception Out_of_bounds

let grid_get_opt grid (x, y) =
  if 0 <= y && y < Array.length grid
  then let line = grid.(y) in
       if 0 <= x && x < Array.length line
       then line.(x)
       else raise Out_of_bounds
  else raise Out_of_bounds

let grid_print grid =
  Array.iter (fun line ->
    Array.iter (function
      | Empty -> Format.printf "."
      | Full -> Format.printf "#"
      | Guard s ->
        let ns = DirectionSet.mem N s || DirectionSet.mem S s in
        let ew = DirectionSet.mem E s || DirectionSet.mem W s in
        if ns && ew then
        Format.printf "+"
        else if ns then Format.printf "|"
        else Format.printf "-"
    ) line;
    Format.printf "@.") grid


let rec patrol grid direction positions pos =
  let newpos = pos +| offset direction in
  match grid_get_opt grid newpos with
  | Full -> patrol grid (rotate_right direction) positions pos
  | Empty ->
      grid.(snd newpos).(fst newpos) <- Guard (DirectionSet.singleton direction);
      patrol grid direction (positions+1) newpos
  | (Guard s) ->
      if DirectionSet.mem direction s
      then None (* Loop *)
      else (
        grid.(snd newpos).(fst newpos) <- Guard (DirectionSet.add direction s);
        patrol grid direction positions newpos
      )
  | exception Out_of_bounds -> Some positions

let grid_foldi (f : int * int -> 'a -> 'acc -> 'acc) (init: 'acc) (a : 'a array array) =
  Array.fold_left (fun (acc, y) line ->
    Array.fold_left (fun s elt ->
      let (acc, x) = s in
      f (x,y) elt acc, x+1)
    (acc, 0) line |> fst, y+1)
  (init, 0) a
  |> fst


let main () =
  let grid = parse_grid (read_all_lines []) in
  let grid_copy = Array.map Array.copy grid in
  let startpos = Option.get !startpos in
  let p1 = Option.get @@ patrol grid N 1 startpos in
  Format.printf "Part 1 : %d@." p1;
  let p2 = grid_foldi (fun pos elt total ->
    match elt with
    | Guard _ ->
      if pos = startpos then total else
        let grid = Array.map Array.copy grid_copy in
        let (x,y) = pos in
        grid.(y).(x) <- Full;
        if patrol grid N 1 startpos = None
        then total + 1
        else total
    | _ -> total (* only place obstruction on guard's path *)) 0 grid
  in
  Format.printf "Part 2 : %d@." p2

let () = main ()
