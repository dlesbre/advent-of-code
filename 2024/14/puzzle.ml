(* ==== Puzzle 14 : https://adventofcode.com/2024/day/14 ====  *)

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

  val dot : t -> t -> int

  val det: t -> t -> int
  (** Determinant of the 2-2 matrix made of both vectors *)

  val inverse: t -> t -> t * t
  (** Inverse of the matrix (not multiplied by 1/det!) *)

  val pp : Format.formatter -> t -> unit
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

  val get: 'a t -> Vec2.t -> 'a (** @raises Out_of_bounds when invalid *)
  val get_opt: 'a t -> Vec2.t -> 'a option
  val copy: 'a t -> 'a t
  val in_bounds: 'a t -> Vec2.t -> bool

  val set: 'a t -> Vec2.t -> 'a -> unit (** @raises Out_of_bounds when invalid *)

  val lines: 'a t -> int

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

  let lines = Array.length

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


type robot = {
  position: Vec2.t;
  velocity: Vec2.t;
}

let parse_line s = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun px py vx vy -> { position=(px,py); velocity=(vx,vy) })

let rec read_all_lines acc =
  try read_all_lines (read_line ()::acc)
  with End_of_file -> List.rev_map parse_line acc

let calc_position steps size robot =
  let open Vec2 in
  (robot.position +| steps *| robot.velocity) %| size

let test_size = (11,7)
let true_size = (101,103)



let quadrant_sum size robot_positions =
  let mid_x = fst size / 2 in
  let mid_y = snd size / 2 in
  let (qNW, qNE, qSW, qSE) = List.fold_left (fun (qNW, qNE, qSW, qSE) robot ->
    if fst robot < mid_x then
        if snd robot < mid_y then
          (qNW+1, qNE, qSW, qSE)
        else if snd robot > mid_y then
          (qNW, qNE, qSW+1, qSE)
        else
          (qNW, qNE, qSW, qSE)
    else if fst robot > mid_x then
      if snd robot < mid_y then
        (qNW, qNE+1, qSW, qSE)
      else if snd robot > mid_y then
        (qNW, qNE, qSW, qSE+1)
      else
        (qNW, qNE, qSW, qSE)
    else
      (qNW, qNE, qSW, qSE)
    ) (0,0,0,0) robot_positions in
  qNW * qNE * qSE * qSW

let to_grid robots size =
  let grid = Grid.make size 0 in
  List.iter (fun pos ->
    Grid.set grid pos (Grid.get grid pos + 1)
    ) robots;
  grid

exception Not_Symmetric

let is_symmetric grid (size_x,_) =
  match Grid.iteri (fun (px,py) nb ->
    if Grid.get grid (size_x - px - 1, py) = nb
    then ()
    else raise Not_Symmetric
  ) grid with
  | () -> true
  | exception Not_Symmetric -> false

let rec list_print_6 a b c d e f= match a,b,c,d,e,f with
  | [],[],[],[],[],[] -> ()
  | a::a', b::b', c::c', d::d', e::e', f::f' ->
     Format.printf "%s    |    %s    |    %s    |    %s    |    %s    |    %s@." a b c d e f;
     list_print_6 a' b' c' d' e' f'
  | _ -> failwith "Mismatch lenghts"

let break_lines = String.split_on_char '\n'

(* With minimal font, I can print 6 grids side-by-side on my terminal... *)
let rec find_tree robots size n =
  let grid_1 = to_grid (List.map (calc_position n size) robots) size in
  let grid_2 = to_grid (List.map (calc_position (n+1) size) robots) size in
  let grid_3 = to_grid (List.map (calc_position (n+2) size) robots) size in
  let grid_4 = to_grid (List.map (calc_position (n+3) size) robots) size in
  let grid_5 = to_grid (List.map (calc_position (n+4) size) robots) size in
  let grid_6 = to_grid (List.map (calc_position (n+5) size) robots) size in
  let s1 = Format.asprintf "Grid at step %d:@.%a@." n (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_1 in
  let s2 = Format.asprintf "Grid at step %d:@.%a@." (n+1) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_2 in
  let s3 = Format.asprintf "Grid at step %d:@.%a@." (n+2) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_3 in
  let s4 = Format.asprintf "Grid at step %d:@.%a@." (n+3) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_4 in
  let s5 = Format.asprintf "Grid at step %d:@.%a@." (n+4) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_5 in
  let s6 = Format.asprintf "Grid at step %d:@.%a@." (n+5) (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid_6 in
  list_print_6 (break_lines s1) (break_lines s2) (break_lines s3) (break_lines s4) (break_lines s5) (break_lines s6);
  Unix.sleepf 0.2;
  find_tree robots size (n+6)

let rec explore_component grid seen elt pos =
  let rec depth_explore pos elt area =
    match elt with
    | 0 -> area
    | _ -> if Hashtbl.mem seen pos then area
           else (
              Hashtbl.add seen pos ();
              Grid.fold_adjacent (fun _ -> depth_explore) (area+1) pos grid)
  in depth_explore pos elt 0


let rec find_tree_smart robots size n =
  let grid = to_grid (List.map (calc_position n size) robots) size in
  let seen = Hashtbl.create (let n = Grid.lines grid in n*n) in
  let max_size = Grid.foldi (fun pos elt max_size ->
    if elt = 0 then max_size
    else max max_size (explore_component grid seen elt pos)) 0 grid in
  if max_size > 100 then
    Format.printf "Grid at step %d:@.%a@." n (Grid.pp (fun fmt n -> if n = 0 then Format.fprintf fmt " " else Format.fprintf fmt "#")) grid
  else find_tree_smart robots size (n+1)



let main () =
  let robots = read_all_lines [] in
  let size = true_size in
  let new_positions = List.map (calc_position 100 size) robots in
  Format.printf "Part 1 : %d@." (quadrant_sum size new_positions);
  find_tree_smart robots size 7000

let () = main ()
