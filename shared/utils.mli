val ( % ) : int -> int -> int
(** Euclidian modulo *)

val read_all_lines : unit -> string list
(** Read all lines from standard input *)

val list_assoc_update: ('b option -> 'b) -> 'a -> ('a*'b) list -> ('a*'b) list
(** [list_assoc_update f a l] replaces binding [(a,b)] in [l] by [(a,f (Some b))]
    If no such binding exists, adds [(a,f None)] at the end of the list. *)

val array_get_opt: 'a array -> int -> 'a option
(** None if out of bounds *)

val set_fold_pairs:
  (module Set.S with type elt = 'elt and type t = 't) ->
  ('elt -> 'elt -> 'acc -> 'acc) -> 't -> 'acc -> 'acc
(** Iterate on the distinct pairs of a set *)

val register:
  year:int ->
  day:int ->
  preprocess:(string list -> 'a) ->
  part1:('a -> string) ->
  part2:('a -> string) ->
  unit

val register_int:
  year:int ->
  day:int ->
  preprocess:(string list -> 'a) ->
  part1:('a -> int) ->
  part2:('a -> int) ->
  unit

val run_solution: year:int -> day:int -> unit
