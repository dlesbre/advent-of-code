module type S = sig
  type elt
  type t
  val empty: t
  val singleton: elt -> t
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
end

module Make(Elt:sig
  type t
  val singleton: t -> int
end) : S with type elt = Elt.t = struct
  type t = int
  type elt = Elt.t
  let singleton = Elt.singleton
  let empty = 0
  let add x s = singleton x lor s
  let mem x s = singleton x land s <> 0
  let remove x s = s land (lnot (singleton x))
  let union a b = a lor b
  let inter a b = a land b
end

module MakeGeneric(Elt: sig type t end) = Make(struct
  include Elt
  let singleton x =
    let repr = Obj.repr x in
    assert (Obj.tag repr = Obj.int_tag);
    let x: int = Obj.magic x in
    assert (0 <= x && x <= 63);
    1 lsl x
end)
