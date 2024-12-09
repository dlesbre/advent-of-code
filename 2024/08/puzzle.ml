(* ==== Puzzle 08 : https://adventofcode.com/2024/day/8 ====  *)

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

type cell = Empty | Antenna of char

module Vec2Set = Set.Make(Vec2)

(** Iterate on pairs of different elements in a set *)
let fold_pairs f set acc =
  Vec2Set.fold (fun elt acc ->
    match Vec2Set.to_seq_from elt set () with
    | Seq.Nil -> acc
    | Seq.Cons(_,s) -> Seq.fold_left (fun acc elt' -> f elt elt' acc) acc s
    ) set acc

(** Given a set of same-labeled antenna position, return the set of their antinodes *)
let find_antinodes set =
  let open Vec2 in
  fold_pairs (fun a b antinodes ->
    let diff = b -| a in
    (* Format.printf "%a %a : %a and %a@." pp a pp b pp (a -| diff) pp (b +| diff); *)
    antinodes |>
    Vec2Set.add (a -| diff) |>
    Vec2Set.add (b +| diff)
  ) set Vec2Set.empty

let rec add_while_in_bounds grid set x diff =
  if Grid.in_bounds grid x
  then add_while_in_bounds grid (Vec2Set.add x set) Vec2.(x +| diff) diff
  else set

let find_resonant_antinodes grid set =
  let open Vec2 in
  fold_pairs (fun a b antinodes ->
    let diff = b -| a in
    (* Format.printf "%a %a : %a and %a@." pp a pp b pp (a -| diff) pp (b +| diff); *)
    let antinodes = add_while_in_bounds grid antinodes (a -| diff) (~| diff) in
    add_while_in_bounds grid antinodes (b +| diff) diff
  ) set
  (if Vec2Set.cardinal set = 1 then Vec2Set.empty else set)

let main () =
  let grid = Grid.read (fun _ c -> if c='.' then Empty else Antenna c) in
  (* Build a map Antenna id -> set of position where it occurs *)
  let table = Hashtbl.create 50 in
  Grid.iteri (fun pos elt -> match elt with
    | Empty -> ()
    | Antenna c -> match Hashtbl.find table c with
        | s -> Hashtbl.replace table c (Vec2Set.add pos s)
        | exception Not_found -> Hashtbl.add table c (Vec2Set.singleton pos)
  ) grid;
  let antinodes = Hashtbl.fold (fun _ s antinodes ->
    Vec2Set.union (find_antinodes s) antinodes
    ) table Vec2Set.empty in
  let antinodes = Vec2Set.filter (Grid.in_bounds grid) antinodes in
  Format.printf "Part 1 : %d@." (Vec2Set.cardinal antinodes);

  let resonant_antinodes = Hashtbl.fold (fun _ s antinodes ->
    Vec2Set.union (find_resonant_antinodes grid s) antinodes
    ) table Vec2Set.empty in
  let resonant_antinodes = Vec2Set.filter (Grid.in_bounds grid) resonant_antinodes in
  Format.printf "Part 2 : %d@." (Vec2Set.cardinal resonant_antinodes)

let () = main ()
