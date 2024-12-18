(** Function manipulating two coordinate vectors *)

type t = int * int

val compare: t -> t -> int

val of_string: string -> t
(** Format "x,y" *)

val in_box: t -> low:t -> high:t -> bool
(** [in_box t ~low ~high] is true iff [low.x <= t.x <= high.x] and same for y *)

val ( +| ) : t -> t -> t
val ( -| ) : t -> t -> t
val ( ~| ) : t -> t
(** unary minus *)

val ( *| ) : int -> t -> t
val ( %| ) : t -> t -> t

val norm_manhattan : t -> int
val dist_manhattan : t -> t -> int

val dot: t -> t -> int

val double_area: t list -> int
(** [double_area polygon] returns the double of the area of the polygon delimited by
    the points of [polygon] (as it is guaranteed to be an integer).
    [polygon] must have at least three points.
    Uses the {{: https://en.wikipedia.org/wiki/Shoelace_formula}Shoelace formula}. *)

val det: t -> t -> int
(** Determinant of the 2-2 matrix made of both vectors *)

val inverse: t -> t -> t * t
(** Inverse of the matrix (not multiplied by 1/det!) *)

val pp: Format.formatter -> t -> unit

module Set: Set.S with type elt = t
