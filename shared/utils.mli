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

type binary_search_result =
  | Present of int
  | Absent of int (** smallest index larger than value *)

val binary_search: (int -> int) -> int -> int -> binary_search_result
(** [binary_search f low high] tries to find a value in [lo..high] (both inclusive)
    that satifies [f].

    [f] uses the convention of [compare]: negative if too low, [0] if correct,
    positive if too high. It should be monotone

    @returns [Present n] if there an [n] with [low <= n <= high] and [f n = 0]
    @returns [Absent n] if there is no [n] in [low..high] such that [f n = 0]
      In that case, [n] is the lowest elemnt of [low..high] such that [f n > 0].
*)

module IntMap : Map.S with type key = int

val imap_pop_minimum: 'a list IntMap.t -> int * 'a * 'a list IntMap.t
(** Return and remove the head element of the list with the lowest key in IntMap *)


val test: bool ref
(** True when running on test input, false otherwise *)

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
