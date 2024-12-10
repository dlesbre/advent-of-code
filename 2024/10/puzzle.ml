(* ==== Puzzle 10 : https://adventofcode.com/2024/day/10 ====  *)

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

  val parse: (Vec2.t -> char -> 'a) -> string list -> 'a t
  (** Transform a string list into a grid, one character per column *)

  val read: (Vec2.t -> char -> 'a) -> 'a t

  val iteri: (Vec2.t -> 'a -> unit) -> 'a t -> unit
  val foldi: (Vec2.t -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
  val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val ppi: (Vec2.t -> Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end = struct
  type 'a t = 'a array array

  exception Out_of_bounds

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
end

module Direction = struct
  type t = N | E | S | W
  let offset = function
    | N ->  ( 0,-1)
    | E ->  ( 1, 0)
    | S ->  ( 0, 1)
    | W ->  (-1, 0)

  let all_directions = [N;E;S;W]
  let fold f acc = List.fold_left f acc all_directions
end

module Vec2Set = Set.Make(Vec2)

(** Score for part 1 *)
let trail_score_1 grid pos =
  let rec follow_trail pos n (total, seen) =
    Direction.fold (fun ((total,seen) as acc) dir ->
      let npos = Vec2.(pos +| Direction.offset dir) in
      if Vec2Set.mem npos seen then acc
      else
        match Grid.get grid npos with
        | x when x = n ->
            let seen = Vec2Set.add npos seen in
            if n = 9
            then (total+1, seen)
            else follow_trail npos (n+1) (total, seen)
        | _ -> acc
        | exception Grid.Out_of_bounds -> acc
      ) (total, seen)
  in follow_trail pos 1 (0, Vec2Set.singleton pos) |> fst

module Vec2Map = Map.Make(Vec2)

let map_incr pos n map = Vec2Map.update pos (function None -> Some n | Some m -> Some (m+n)) map
let find pos map = match Vec2Map.find pos map with
  | n -> n
  | exception Not_found -> 0

(** Score for part 2 *)
let trail_score_2 grid pos =
  let rec follow_trail pos n paths =
    Direction.fold (fun paths dir ->
      let npos = Vec2.(pos +| Direction.offset dir) in
      match Grid.get grid npos with
      | x when x = n ->
          if Vec2Map.mem npos paths
          then map_incr pos (find npos paths) paths
          else if n = 9
            then map_incr pos 1 paths
            else
              let paths = follow_trail npos (n+1) paths in
              map_incr pos (find npos paths) paths
      | _ -> paths
      | exception Grid.Out_of_bounds -> paths
      ) paths
  in follow_trail pos 1 Vec2Map.empty
  |> find pos

let main () =
  let grid = Grid.read (fun _ c -> int_of_char c - int_of_char '0') in
  let p1 = Grid.foldi (fun pos elt acc ->
    if elt = 0
    then acc + trail_score_1 grid pos
    else acc) 0 grid in
  Format.printf "Part 1 : %d@." p1;
  let p2 = Grid.foldi (fun pos elt acc ->
    if elt = 0
    then acc + trail_score_2 grid pos
    else acc) 0 grid in
  Format.printf "Part 2 : %d@." p2

let () = main ()
