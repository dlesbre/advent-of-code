type t = {
  start: int; (** included *)
  stop: int;  (** excluded *)
  step: int
}

val upto: int -> t
(** [upto x] is range [0,1,..,x-1] *)

val interval: int -> int -> t
(** [interval los high] is range [lo, lo+1, .., high-1] *)

val iter: (int -> unit) -> t -> unit
val fold: (int -> 'acc -> 'acc) -> t -> 'acc -> 'acc
val forall: (int -> bool) -> t -> bool
val exists: (int -> bool) -> t -> bool
val sum: (int -> int) -> t -> int
(** [sum f r] is the [f r.start + f (r.start+r.step) + ...] *)

val count: (int -> bool) -> t -> int
(** [count f r] counts the number of elements of r that satisfy [f] *)
