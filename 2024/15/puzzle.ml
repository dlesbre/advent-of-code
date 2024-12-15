(* ==== Puzzle 15 : https://adventofcode.com/2024/day/15 ====  *)

(* Euclidian modulo *)
let ( % ) x y =
  let a = x mod y in
  if a < 0 then a + y else a

module Vec2 : sig
  type t = int * int

  val compare: t -> t -> int

  val ( +| ) : t -> t -> t
  val ( -| ) : t -> t -> t
  val ( ~| ) : t -> t (** unary minus *)
  val ( *| ) : int -> t -> t
  val ( %| ) : t -> t -> t

  val norm_manhattan : t -> int
  val dist_manhattan : t -> t -> int

  val dot: t -> t -> int

  val det: t -> t -> int
  (** Determinant of the 2-2 matrix made of both vectors *)

  val inverse: t -> t -> t * t
  (** Inverse of the matrix (not multiplied by 1/det!) *)

  val pp: Format.formatter -> t -> unit
end = struct
  type t = int * int

  let compare (x,y) (x',y') =
    let cmp = Int.compare x x' in
    if cmp = 0 then Int.compare y y' else cmp

  let ( +| ) (x,y) (x',y') = x+x', y+y'
  let ( -| ) (x,y) (x',y') = x-x', y-y'
  let ( ~| ) (x,y) = (-x,-y)
  let ( *| ) n (x,y) = n*x, n*y
  let ( %| ) (x,y) (x',y') = (x % x', y % y')

  let norm_manhattan (x,y) = abs x + abs y
  let dist_manhattan a b = a -| b |> norm_manhattan

  let pp fmt (x,y) = Format.fprintf fmt "(%d,%d)" x y

  let dot (x,y) (x',y') = x*x' + y*y'

  let det (x,y) (x',y') = x*y' - y*x'

  let inverse (x,y) (x',y') = (y',-y), (-x',x)
end


module Grid : sig
  type 'a t

  exception Out_of_bounds

  val make: Vec2.t -> 'a -> 'a t
  val init: Vec2.t -> (Vec2.t -> 'a) -> 'a t

  val get: 'a t -> Vec2.t -> 'a (** @raises Out_of_bounds when invalid *)
  val get_opt: 'a t -> Vec2.t -> 'a option
  val copy: 'a t -> 'a t
  val in_bounds: 'a t -> Vec2.t -> bool

  val set: 'a t -> Vec2.t -> 'a -> unit (** @raises Out_of_bounds when invalid *)

  val lines: 'a t -> int
  val columns: 'a t -> int
  val size: 'a t -> Vec2.t

  val parse: (Vec2.t -> char -> 'a) -> string list -> 'a t
  (** Transform a string list into a grid, one character per column *)

  val read: (Vec2.t -> char -> 'a) -> 'a t

  val iteri: (Vec2.t -> 'a -> unit) -> 'a t -> unit
  val foldi: (Vec2.t -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val ppi: (Vec2.t -> Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  type direction = N | E | S | W
  val direction_pp : Format.formatter -> direction -> unit
  val vec2_of_direction : direction -> Vec2.t

  val fold_adjacent: (direction -> Vec2.t -> 'a -> 'acc -> 'acc) -> 'acc -> Vec2.t -> 'a t -> 'acc
  (** Folds the given function on the four adjacent points (if they exist) *)

  val fold_adjacent_opt: (direction -> Vec2.t -> 'a option -> 'acc -> 'acc) -> 'acc -> Vec2.t -> 'a t -> 'acc
  (** Same as [fold_adjacent], but also called on positions outside of the grid (with value [None]) *)
end = struct
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

  let pp pp_a fmt grid =
    Array.iter (fun line ->
      Array.iter (pp_a fmt) line;
      Format.fprintf fmt "@.") grid

  let ppi pp_a fmt grid =
    Array.iteri (fun y line ->
      Array.iteri (fun x elt -> pp_a (x,y) fmt elt) line;
      Format.fprintf fmt "@.") grid

  type direction = N | E | S | W
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

  let all_directions = [N;E;S;W]

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
end

type item = Wall | Box of bool (* true for left box *) | Robot | Empty

let robot_init = ref None

let parse_item pos = function
  | '#' -> Wall
  | 'O' -> Box true (* dummy value *)
  | '@' -> assert (!robot_init = None); robot_init:=Some pos; Robot
  | '.' -> Empty
  | _ -> failwith "Wrong format"

let parse_direction = function
  | '^' -> Grid.N
  | '>' -> Grid.E
  | 'v' -> Grid.S
  | '<' -> Grid.W
  | _ -> failwith "Wrong format"

let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev acc

let rec list_split_on f = function
  | [] -> failwith "wrong format"
  | x::xs when f x -> [], xs
  | x::xs -> let a,b= list_split_on f xs in x::a, b

let parse () =
  let lines = read_all_lines [] in
  let grid, dirs = list_split_on (fun x -> x = "") lines in
  let grid = Grid.parse parse_item grid in
  let dirs = List.map (fun s -> List.of_seq (String.to_seq s)) dirs
    |> List.flatten
    |> List.map parse_direction in
  grid, dirs

(** Move box for part 1:
    Move a box the robot is pushing, or a stack of boxes
    Does not move the robot, returns true if the box was moved
    Here pos_init is position of the first box of the row, which we will teleport
    to the first empty space found. *)
let rec move_box_1 grid pos_init pos_target offset b =
  match Grid.get grid pos_target with
  | Empty ->
      Grid.set grid pos_target (Box true);
      Grid.set grid pos_init Empty;
      true
  | Wall -> false
  | Box _ -> move_box_1 grid pos_init Vec2.(pos_target +| offset) offset b
  | Robot -> failwith "Moving box into robot"

(** For part 2, moving horizontally is similar to P1, but because L/R is distinguished,
    we can't just teleport the first box to the last position, we need to update the whole row *)
let rec move_box_2_horizontal grid pos_init pos_target offset =
  match Grid.get grid pos_target with
  | Empty ->
      Grid.set grid pos_target (Grid.get grid pos_init);
      true
  | Wall -> false
  | Box _ ->
      let has_moved = move_box_2_horizontal grid pos_target Vec2.(pos_target +| offset) offset in
      if has_moved then Grid.set grid pos_target (Grid.get grid pos_init);
      has_moved
  | Robot -> failwith "Moving box into robot"

(** Find positions of both halves of a box from a single half *)
let box_pos pos b =
  let open Vec2 in
  if b
  then (pos, pos +| Grid.vec2_of_direction Grid.E)
  else Vec2.(pos +| Grid.vec2_of_direction Grid.W, pos)

(** Check if we can move box vertically *)
let rec can_move_box_2_vertical grid pos_init pos_target offset =
  let open Vec2 in
  match Grid.get grid pos_target with
  | Empty -> true
  | Wall -> false
  | Box b ->
      let box_pos_l, box_pos_r = box_pos pos_target b in
      can_move_box_2_vertical grid box_pos_l (box_pos_l +| offset) offset &&
      can_move_box_2_vertical grid box_pos_r (box_pos_r +| offset) offset
  | Robot -> failwith "Moving box into robot"

(** This recursively moves all boxes. Assumes such a move is possible *)
let rec perform_move_box_2_vertical grid pos_init pos_target offset =
  let open Vec2 in
  begin match Grid.get grid pos_target with
  | Box b ->
      let box_pos_l, box_pos_r = box_pos pos_target b in
      perform_move_box_2_vertical grid box_pos_l (box_pos_l +| offset) offset;
      perform_move_box_2_vertical grid box_pos_r (box_pos_r +| offset) offset;
  | _ -> ()
  end;
  Grid.set grid pos_target (Grid.get grid pos_init);
  Grid.set grid pos_init Empty

let move_box_2 grid pos_init pos_target ((x,y) as offset) b =
  let open Vec2 in
  if y = 0 then
    move_box_2_horizontal grid pos_init pos_target offset
  else begin
    let box_pos_l, box_pos_r = box_pos pos_init b in
    let can_move =
      can_move_box_2_vertical grid box_pos_l (box_pos_l +| offset) offset &&
      can_move_box_2_vertical grid box_pos_r (box_pos_r +| offset) offset in
    if can_move then (
      perform_move_box_2_vertical grid box_pos_l (box_pos_l +| offset) offset;
      perform_move_box_2_vertical grid box_pos_r (box_pos_r +| offset) offset;
    );
    can_move
  end

let move_robot move_box grid pos direction =
  let offset = Grid.vec2_of_direction direction in
  let npos = Vec2.(pos +| offset) in
  match Grid.get grid npos with
  | Empty ->
      Grid.set grid pos Empty;
      Grid.set grid npos Robot;
      npos
  | Wall -> pos
  | Box b ->
      if move_box grid npos Vec2.(npos +| offset) offset b then (
          Grid.set grid pos Empty;
          Grid.set grid npos Robot;
          npos)
      else pos
  | Robot -> failwith "Duplicate robot"


let grid_score grid =
  Grid.foldi (fun (x,y) elt total ->
    match elt with
    | Box true -> 100*y + x + total
    | _ -> total) 0 grid


let main () =
  let grid, dirs = parse () in
  let (x,y) = Grid.size grid in
  let duplicate_grid = Grid.init (2*x,y) (fun (x,y) ->
    match Grid.get grid (x/2, y) with
    | Empty -> Empty
    | Wall -> Wall
    | Robot -> if x mod 2 = 0 then Robot else Empty
    | Box _ -> Box (x mod 2 = 0)) in
  let pos = Option.get !robot_init in
  let _final_pos = List.fold_left (move_robot move_box_1 grid) pos dirs in
  Format.printf "Part 1 : %d@." (grid_score grid);
  let (x, y) = pos in
  let _final_pos = List.fold_left (move_robot move_box_2 duplicate_grid) (2*x,y) dirs in
  Format.printf "Part 2 : %d@." (grid_score duplicate_grid)



let () = main ()
