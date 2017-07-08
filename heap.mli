module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type H =
  sig
    type elt
    type t
    exception Empty
    val empty: t
    val insert: elt -> t -> t
    val pop: t -> t
    val top: t -> elt
  end

module Make (Ord: OrderedType) : H with type elt = Ord.t
