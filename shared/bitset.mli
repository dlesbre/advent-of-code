(** A bit set is a compact way to represent sets of a small type (which has less
    than 63 eleemnts). A single int represent the whole set, with each bit
    representing an element. As a result all operations are constant time.
    However, iteration/find_min is not possible. *)

module type S = sig
  type elt
  type t

  val empty: t
  val singleton: elt -> t
  val of_list: elt list -> t

  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val disjoint_union: t -> t -> t
  (** [disjoint_union l r] is the set of elements that are in either [l] or [r],
      but not both *)
end

module Make(Elt:sig
  type t
  val singleton: t -> int
  (** Should map each element to a separate power of 2*)
end) : S with type elt = Elt.t

module MakeGeneric(Elt: sig
  type t
  (** should only have constant constructors *)
end) : S with type elt = Elt.t
