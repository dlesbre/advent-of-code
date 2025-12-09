(** Function manipulating two coordinate vectors *)

type t = int * int

val compare: t -> t -> int

val of_string: string -> t
(** Format "x,y" *)

val in_box: t -> low:t -> high:t -> bool
(** [in_box t ~low ~high] is true iff [low.x <= t.x <= high.x] and same for y *)

val bounding_box: t list -> t * t
(** Returns the smallest bounding box containing all items in the (non-empty) argument list,
    i.e. [low, high] such that [List.forall (in_box ~low ~high) list].
    the first point's coordinates are both smaller than the second's*)

val intersect_x_line: x:int -> y1:int -> y2:int -> low:t -> high:t -> bool
(** Test if the line [(x,y1) -- (x,y2)] inclusive intersects the bounding box *)

val intersect_y_line: y:int -> x1:int -> x2:int -> low:t -> high:t -> bool
(** Test if the line [(x1,y) -- (x2,y)] inclusive intersects the bounding box *)

val ( +| ) : t -> t -> t
val ( -| ) : t -> t -> t
val ( ~| ) : t -> t (** unary minus *)

val ( *| ) : int -> t -> t
val ( %| ) : t -> t -> t (** Pointwise modulo*)

val norm_manhattan : t -> int
val dist_manhattan : t -> t -> int

val dot: t -> t -> int

val det: t -> t -> int
(** Determinant of the 2-2 matrix made of both vectors *)

val inverse: t -> t -> t * t
(** Inverse of the matrix (not multiplied by 1/det to remain in integers) *)

val pp: Format.formatter -> t -> unit

module Set: Set.S with type elt = t
