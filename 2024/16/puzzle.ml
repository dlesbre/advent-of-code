(* ==== Puzzle 16 : https://adventofcode.com/2024/day/16 ====  *)


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
  val rotate_clockwise: direction -> direction (** N -> E *)
  val rotate_counterclockwise: direction -> direction (** N -> W *)
  val direction_fold: (direction -> 'acc -> 'acc) -> 'acc -> 'acc

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
end

type tile = Wall | Empty

let startpos = ref None
let endpos = ref None

let parse_tile pos = function
  | '#' -> Wall
  | '.' -> Empty
  | 'S' -> startpos := Some pos; Empty
  | 'E' -> endpos := Some pos; Empty
  | _ -> failwith "Invalid input"


(** Use a map score -> list of positions as a priority queue *)
module IntMap = Map.Make(Int)
module Vec2Set = Set.Make(Vec2)

let pop_minimum imap = match IntMap.min_binding imap with
  | (i, []) -> failwith "empty min"
  | (i, [n]) -> i, n, IntMap.remove i imap
  | (i, n::ns) -> i, n, IntMap.add i ns imap

let rec list_assoc_update f n = function
  | [] -> [n, f None]
  | (n', a)::ns when n = n' -> (n, f (Some a))::ns
  | elt::ns -> elt::list_assoc_update f n ns

let add_elt i n path imap =
  IntMap.update i (function
    | None -> Some [(n, path)]
    | Some ns -> Some (list_assoc_update (function None -> path | Some p -> Vec2Set.union p path) n ns))
  imap

(** Perform breadth first search using a priority queue "score -> position*direction*paths" *)
let rec bfs grid seen queue =
  let (score, (n, path), queue) = pop_minimum queue in
  if Hashtbl.mem seen n then bfs grid seen queue
  else
    let (pos, dir) = n in
    if pos = Option.get !endpos then score, path
    else begin
      Hashtbl.replace seen (pos,dir) ();
      let npos = Vec2.(pos +| Grid.vec2_of_direction dir) in
      begin match Grid.get grid npos with
      | Empty -> add_elt (score+1) (npos,dir) (Vec2Set.add npos path) queue
      | _ -> queue
      end |>
      add_elt (score+1000) (pos, (Grid.rotate_clockwise dir)) path |>
      add_elt (score+1000) (pos, (Grid.rotate_counterclockwise dir)) path |>
      bfs grid seen
    end

let main () =
  let grid = Grid.read parse_tile in
  let startpos = Option.get !startpos in
  let table = Hashtbl.create (let n = Grid.lines grid in n*n) in
  let score, paths = bfs grid table @@ IntMap.singleton 0 [(startpos, Grid.E), Vec2Set.singleton startpos] in
  Format.printf "Part 1 : %d@." score;
  Format.printf "Part 2 : %d@." (Vec2Set.cardinal paths)


let () = main ()
