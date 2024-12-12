(* ==== Puzzle 12 : https://adventofcode.com/2024/day/12 ====  *)

module Vec2 : sig
  type t = int * int

  val compare: t -> t -> int

  val ( +| ) : t -> t -> t
  val ( -| ) : t -> t -> t
  val ( ~| ) : t -> t (** unary minus *)
  val ( *| ) : int -> t -> t

  val norm_manhattan : t -> int
  val dist_manhattan : t -> t -> int

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

  let norm_manhattan (x,y) = abs x + abs y
  let dist_manhattan a b = a -| b |> norm_manhattan

  let pp fmt (x,y) = Format.fprintf fmt "(%d,%d)" x y
end

module Grid : sig
  type 'a t

  exception Out_of_bounds

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

module Vec2Set = Set.Make(Vec2)

let rec explore_component_1 grid seen char pos =
  let rec depth_explore pos elt (perimeter, area) =
    match elt with
    | None -> (perimeter+1, area)
    | Some n when n = char ->
              if Hashtbl.mem seen pos then (perimeter, area)
              else (
                Hashtbl.add seen pos ();
                Grid.fold_adjacent_opt (fun _ -> depth_explore) (perimeter, area+1) pos grid)
    | Some _ -> (perimeter+1, area)
  in depth_explore pos (Some char) (0,0)

let explore_with explore grid =
  let seen = Hashtbl.create (let n = Grid.lines grid in n*n) in
  Grid.foldi (fun pos elt sum ->
    if Hashtbl.mem seen pos then sum
    else
      let (perimeter, area) = explore grid seen elt pos in
      sum + perimeter * area
    ) 0 grid





(** Given a list of segment (number i represent segment i..i+1) on a single line
    [nb_sides list] is the number of connected components (merging adjacent segments) *)
let nb_sides list =
  let rec aux previous total = function
    | [] -> total
    | x::rs when x = previous + 1 -> aux x total rs
    | x::rs -> aux x (total + 1) rs
  in aux (-5) 0 (List.sort Int.compare list)
  (* -5 is guaranteed to be smaller than all elements *)

let id_of_direction dir (x,y) =
  let open Grid in
  match dir with
  | N | S -> (dir, y), x
  | E | W -> (dir, x), y

let hashtbl_add_or_incr table pos x = match Hashtbl.find table pos with
  | n -> Hashtbl.replace table pos (x::n)
  | exception Not_found -> Hashtbl.add table pos [x]

let pp fmt l = Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ") Format.pp_print_int) l

let rec explore_component_2 grid seen char pos =
  let sides = Hashtbl.create 50 in (* map (int, direction) -> List of sides there *)
  let rec depth_explore dir pos elt area =
    match elt with
    | Some n when n = char ->
              if Hashtbl.mem seen pos then area
              else (
                Hashtbl.add seen pos ();
                Grid.fold_adjacent_opt depth_explore (area+1) pos grid)
    | Some _ | None ->
          let id, p = id_of_direction dir pos in
          hashtbl_add_or_incr sides id p; area
  in
  let area = depth_explore (Grid.N) pos (Some char) 0 in
  Hashtbl.fold (fun (d,n) l total -> total + nb_sides l) sides 0, area


let main () =
  let input = Grid.read (fun _ -> Fun.id) in
  Format.printf "Part 1 : %d@." (explore_with explore_component_1 input);
  Format.printf "Part 2 : %d@." (explore_with explore_component_2 input)



let () = main ()
