(* Bunch of useful function to solve any puzzle, mainly to parse input *)

(** Returns the suffix of str starting at start *)
let string_suffix str start =
  String.sub str start (String.length str - start)

let parse_line x = x

(** Returns a list of all lines in the input, first line as hd *)
let rec read_all_lines acc =
  try
    let line = read_line () in
    let contents = parse_line line in
    read_all_lines (contents::acc)
  with End_of_file ->
    List.rev acc

(** Return 0, 1 or -1 according to the sign of a - b *)
let sign a b =
  if a = b then 0
  else if a > b then 1
  else -1

let foldi_of_fold fold (f: 'a -> int -> 'b -> 'a) acc l =
  fst (fold (fun (acc,i) x -> (f acc i x, i+1)) (acc,0) l)

let list_foldi f = foldi_of_fold List.fold_left f

module Vec2 = struct
  type t = int * int
  type direction = Up | Down | Left | Right

  let parse_direction = function
    | 'U' -> Up
    | 'D' -> Down
    | 'L' -> Left
    | 'R' -> Right
    | c -> failwith "Wrong direction char"

  let dir2vec2 = function
    | Up ->    ( 1,  0)
    | Down ->  (-1,  0)
    | Left ->  ( 0, -1)
    | Right -> ( 0,  1)

  let x = fst
  let y = snd

  let add (x1,y1) (x2,y2) = (x1+x2, y1+y2)
  let sub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
  let mul (x1,y1) (x2,y2) = (x1*x2, y1*y2)


  let abs (x,y) = abs x, abs y

  let norm_infinite (x,y) = let (x,y) = abs (x,y) in max x y
  let norm_manhattan (x,y) = let (x,y) = abs (x,y) in x + y

  let distance norm a b = norm (sub a b)

  let distance_infinite = distance norm_infinite
  let distance_manhattan = distance norm_manhattan

  let scale s (x,y) = (s*x, s*y)
  let scalar (x1,y1) (x2,y2) = x1 * y2 - x2 * y1

  let add_offset p d = add p (dir2vec2 d)

  module Vec2Not = struct
    let ( +@ ) = add
    let ( -@ ) = sub
    let ( *@ ) = scale
    let ( *. ) = scalar
  end
end
