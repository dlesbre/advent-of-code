(** {1 Useful functions missing from Stdlib}                                  *)
(******************************************************************************)

val ( % ) : int -> int -> int
(** Euclidian modulo *)

val string_suffix: string -> int -> string
(** [string_suffix str n] is the substring of [str] excluding its [n] first characters. *)

val list_assoc_update: ('b option -> 'b) -> 'a -> ('a*'b) list -> ('a*'b) list
(** [list_assoc_update f a l] replaces binding [(a,b)] in [l] by [(a,f (Some b))]
    If no such binding exists, adds [(a,f None)] at the end of the list. *)

val array_get_opt: 'a array -> int -> 'a option
(** None if out of bounds *)

val list_count: ('a -> bool) -> 'a list -> int
(** [list_count f l] is the number of elements of [l] that satisfy [f] *)

val list_sum: ('a -> int) -> 'a list -> int
(** [list_sum f [a0;...;an]] is [f a0 + ... + f an]. *)

val list_min: ('a -> int) -> 'a list -> int

val list_find_first: ('a -> bool) -> 'a list -> ('a * int) option
(** [list_find_first f l] returns the first element of [l] that satifies [f],
    along with its position. *)

val list_split: ('a -> bool) -> 'a list -> 'a list list
(** [list_split f l] splits [l] along elements that satifsy [l] (removing those
    elements from the list).

    For example: [list_split (fun x -> x = 0) [1;2;3;0;0;5;7;0;1;3;0]]
    is [[ [1;2;3]; []; [5;7]; [1;3]; []]] *)


val ( |>> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

val set_fold_pairs:
  (module Set.S with type elt = 'elt and type t = 't) ->
  ('elt -> 'elt -> 'acc -> 'acc) -> 't -> 'acc -> 'acc
(** Iterate on the distinct pairs of a set *)

val set_sum:
  (module Set.S with type elt = 'elt and type t = 't) ->
  ('elt -> int) -> 't -> int
(** [set_sum (module Set) f t] sums the values of [f] applied to all elements of [t]. *)

val set_count:
  (module Set.S with type elt = 'elt and type t = 't) ->
  ('elt -> bool) -> 't -> int
(** [set_count (module Set) f t] counts the elements of [t] that satisfy [f]. *)

val hashtbl_incr: ('a, int) Hashtbl.t -> 'a -> int -> unit
(** [hashtbl_incr table key n] does [table[key] += n], creating [table[key]] if needed *)

val hashtbl_cons: ('a, 'b list) Hashtbl.t -> 'a -> 'b -> unit
(** [hashtbl_cons table key n] does [table[key] = n :: table[key]], creating [table[key]] if absent. *)


(** {1 Integer map as priority queue}                                         *)
(******************************************************************************)

module IntMap : Map.S with type key = int
module IntSet : Set.S with type elt = int

val imap_pop_minimum: 'a list IntMap.t -> int * 'a * 'a list IntMap.t
(** Return and remove the head element of the list with the lowest key in IntMap *)

val imap_add_elt: int -> 'a -> 'a list IntMap.t -> 'a list IntMap.t
(** Add an element with given priority (at the head of a list) *)

val imap_merge_elt: int -> 'a -> ('b option -> 'b) -> ('a*'b) list IntMap.t -> ('a*'b) list IntMap.t
(** [imap_merge_elt p k f imap] adds an element with priority [p] in [imap]:
    - If there is already an element [(k,b)] with priority [p], replace it with [(k, f (Some b))]
    - Else, add [(k, f None)] at the end of the list *)

(** {1 Intify types}                                                         *)
(******************************************************************************)
(** I.E. inject values from a given type to integers by counting them *)

module IntifyOrder(T: Map.OrderedType) : sig val get_id: T.t -> int end
module IntifyHash(T: Hashtbl.HashedType) : sig val get_id: T.t -> int end

(** {1 Mathematical function/algorithms}                                      *)
(******************************************************************************)

(** Both gcd and lcm are guaranteed to be positive *)
val gcd: int -> int -> int
val lcm: int -> int -> int

val bezout_coefficients: int -> int -> int * int * int
(** [bezout_coefficients a b] is [(gcd a b, u, v)] with [a*u + b*v = gcd a b]. *)

type binary_search_result =
  | Present of int
  | Absent of int (** smallest index larger than value *)

val binary_search: (int -> int) -> int -> int -> binary_search_result
(** [binary_search f low high] tries to find a value in [lo..high] (both inclusive)
    that satifies [f].

    [f] uses the convention of [compare]: negative if too low, [0] if correct,
    positive if too high. The sign of [f] must be increasing (i.e. if [n < m]
    then [sign (f n) <= sign (f m)]).

    @returns [Present n] if there an [n] with [low <= n <= high] and [f n = 0].
      If there are multiple such [n], returns an arbitrary choice.
    @returns [Absent n] if there is no [n] in [low..high] such that [f n = 0]
      In that case, [n] is the lowest element of [low..high] such that [f n > 0].
      This assumes [f high >= 0].
      If not, may return [Absent high] with [f high < 0]. *)

val polygon_double_area: (int * int) list -> int
(** [polygon_double_area polygon] returns the double of the area of the polygon delimited by
    the points of [polygon] (as it is guaranteed to be an integer).
    [polygon] must have at least three points.
    Uses the {{: https://en.wikipedia.org/wiki/Shoelace_formula}Shoelace formula}. *)


(** {1 Register and run puzzle solutions}                                     *)
(******************************************************************************)

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
