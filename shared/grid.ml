type 'a t = 'a array array

exception Out_of_bounds

let make (x,y) a = Array.init y (fun _ -> Array.make x a)
let init (x,y) f = Array.init y (fun y -> Array.init x (fun x -> f (x,y)))

let lines = Array.length
let columns x = Array.length (x.(0))
let size x = (lines x, columns x)

let in_bounds grid (x,y) =
  0 <= y && y < Array.length grid &&
  let line = grid.(y) in
  0 <= x && x < Array.length line

let get grid (x, y) =
  if 0 <= y && y < Array.length grid
  then let line = grid.(y) in
       if 0 <= x && x < Array.length line
       then line.(x)
       else raise Out_of_bounds
  else raise Out_of_bounds

let set grid (x, y) v =
  if 0 <= y && y < Array.length grid
  then let line = grid.(y) in
        if 0 <= x && x < Array.length line
        then line.(x) <- v
        else raise Out_of_bounds
  else raise Out_of_bounds

let get_opt grid (x, y) =
  if 0 <= y && y < Array.length grid
  then let line = grid.(y) in
        if 0 <= x && x < Array.length line
        then Some line.(x)
        else None
  else None

let copy x = Array.map Array.copy x

let parse f lines =
  let lines = List.mapi (fun y line ->
    Array.init (String.length line)
    (fun x -> f (x,y) line.[x])) lines in
  Array.of_list lines

let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev acc

let read f = read_all_lines [] |> parse f


let iteri (f : int * int -> 'a -> unit) a =
  Array.iteri (fun y line -> Array.iteri (fun x elt -> f (x,y) elt) line) a

let foldi (f : int * int -> 'a -> 'acc -> 'acc) (init: 'acc) (a : 'a array array) =
  Array.fold_left (fun (acc, y) line ->
    Array.fold_left (fun s elt ->
      let (acc, x) = s in
      f (x,y) elt acc, x+1)
    (acc, 0) line |> fst, y+1)
  (init, 0) a
  |> fst

let sum f grid = foldi (fun pos elt acc -> f pos elt + acc) 0 grid
let count f grid = foldi (fun pos elt acc -> if f pos elt then acc + 1 else acc) 0 grid

let pp pp_a fmt grid =
  Array.iter (fun line ->
    Array.iter (pp_a fmt) line;
    Format.fprintf fmt "@.") grid

let ppi pp_a fmt grid =
  Array.iteri (fun y line ->
    Array.iteri (fun x elt -> pp_a (x,y) fmt elt) line;
    Format.fprintf fmt "@.") grid

type direction = N | E | S | W
module DirectionSet = Bitset.Make(struct
  type t = direction
  let singleton = function
  | N -> 1
  | E -> 2
  | S -> 4
  | W -> 8
end)
let vec2_of_direction = function
  | N ->  ( 0,-1)
  | E ->  ( 1, 0)
  | S ->  ( 0, 1)
  | W ->  (-1, 0)
let direction_pp fmt = function
  | N -> Format.fprintf fmt "N"
  | E -> Format.fprintf fmt "E"
  | S -> Format.fprintf fmt "S"
  | W -> Format.fprintf fmt "W"
  let rotate_clockwise = function
  | N -> E
  | E -> S
  | S -> W
  | W -> N
let rotate_counterclockwise = function
  | N -> W
  | E -> N
  | S -> E
  | W -> S

let all_directions = [N;E;S;W]
let direction_fold f acc = List.fold_left (fun acc d -> f d acc) acc all_directions

let fold_adjacent_opt f acc pos grid =
  let open Vec2 in
  List.fold_left (fun acc elt ->
    let npos = pos +| vec2_of_direction elt in
    f elt npos (get_opt grid npos) acc
    ) acc all_directions

let fold_adjacent f acc pos grid =
  fold_adjacent_opt
    (fun dir pos v acc -> match v with
        | None -> acc
        | Some x -> f dir pos x acc) acc pos grid

type direction8 = NN | NE | EE | SE | SS | SW | WW | NW
module Direction8Set = Bitset.Make(struct
  type t = direction8
  let singleton = function
  | NN -> 1
  | EE -> 2
  | SS -> 4
  | WW -> 8
  | NE -> 16
  | NW -> 32
  | SE -> 64
  | SW -> 128
end)
let direction8_of_4 = function
  | N -> NN
  | E -> EE
  | W -> WW
  | S -> SS

let all_directions = [NN; NE; EE; SE; SS; SW; WW; NW]

let direction8_fold f init = List.fold_left (fun elt acc -> f acc elt) init all_directions

let vec2_of_direction8 = function
  | NN -> ( 0,-1)
  | NE -> ( 1,-1)
  | EE -> ( 1, 0)
  | SE -> ( 1, 1)
  | SS -> ( 0, 1)
  | SW -> (-1, 1)
  | WW -> (-1, 0)
  | NW -> (-1,-1)
let direction8_pp fmt = function
  | NN -> Format.fprintf fmt "N"
  | EE -> Format.fprintf fmt "E"
  | SS -> Format.fprintf fmt "S"
  | WW -> Format.fprintf fmt "W"
  | NE -> Format.fprintf fmt "NE"
  | NW -> Format.fprintf fmt "NW"
  | SE -> Format.fprintf fmt "SE"
  | SW -> Format.fprintf fmt "SW"
