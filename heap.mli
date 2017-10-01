module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    exception Empty
    val empty: t
    val merge: t -> t -> t
    val singleton: elt -> t
    val insert: elt -> t -> t
    val pop: t -> t
    val top: t -> elt
  end

module Make (Ord: OrderedType) : S with type elt = Ord.t
