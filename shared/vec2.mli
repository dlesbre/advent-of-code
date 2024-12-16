(** Function manipulating two coordinate vectors *)

type t = int * int

val compare: t -> t -> int

val ( +| ) : t -> t -> t
val ( -| ) : t -> t -> t
val ( ~| ) : t -> t
(** unary minus *)

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

module Set: Set.S with type elt = t
