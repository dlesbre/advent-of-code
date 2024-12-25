(** Useful function for manipulating grids (2D arrays), as they are recurrent
    in advent of code.
    A grid's coordinates are given as a pair (row,column). *)

type 'a t

exception Out_of_bounds

val make: Vec2.t -> 'a -> 'a t
val init: Vec2.t -> (Vec2.t -> 'a) -> 'a t
val of_array: 'a array array -> 'a t

val get: 'a t -> Vec2.t -> 'a
(** @raises Out_of_bounds when invalid *)

val set: 'a t -> Vec2.t -> 'a -> unit
(** @raises Out_of_bounds when invalid *)

val get_opt: 'a t -> Vec2.t -> 'a option
val copy: 'a t -> 'a t
val in_bounds: 'a t -> Vec2.t -> bool
(** [in_bounds grid pos] is true when pos is a valid position in the grid. *)

val lines: 'a t -> int
val columns: 'a t -> int
val size: 'a t -> Vec2.t

val parse: (Vec2.t -> char -> 'a) -> string list -> 'a t
(** Transform a string list into a grid, one character per column *)

val read: (Vec2.t -> char -> 'a) -> 'a t

val iteri: (Vec2.t -> 'a -> unit) -> 'a t -> unit
val foldi: (Vec2.t -> 'a -> 'acc -> 'acc) -> 'acc -> 'a t -> 'acc
val sum: (Vec2.t -> 'a -> int) -> 'a t -> int
val count: (Vec2.t -> 'a -> bool) -> 'a t -> int
val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val ppi: (Vec2.t -> Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

type direction = N | E | S | W
module DirectionSet: Bitset.S with type elt = direction
val direction_pp : Format.formatter -> direction -> unit
val vec2_of_direction : direction -> Vec2.t
val direction_parse: char -> direction
(** Parse '^', '<', '>', 'v' or 'N', 'E', 'S', 'W' in a direction *)

val rotate_clockwise: direction -> direction (** N -> E *)

val rotate_counterclockwise: direction -> direction (** N -> W *)

val direction_fold: (direction -> 'acc -> 'acc) -> 'acc -> 'acc

val fold_adjacent: (direction -> Vec2.t -> 'a -> 'acc -> 'acc) -> 'acc -> Vec2.t -> 'a t -> 'acc
(** Folds the given function on the four adjacent points (if they exist) *)

val fold_adjacent_opt: (direction -> Vec2.t -> 'a option -> 'acc -> 'acc) -> 'acc -> Vec2.t -> 'a t -> 'acc
(** Same as [fold_adjacent], but also called on positions outside of the grid (with value [None]) *)

type direction8 = NN | NE | EE | SE | SS | SW | WW | NW
module Direction8Set: Bitset.S with type elt = direction8
val direction8_of_4: direction -> direction8
val direction8_fold: (direction8 -> 'acc -> 'acc) -> 'acc -> 'acc
val direction8_pp: Format.formatter -> direction8 -> unit
val vec2_of_direction8: direction8 -> Vec2.t
