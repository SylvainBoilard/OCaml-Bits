module Subarray :
  sig
    type 'a t

    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val fold_lefti : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
  end

type 'a t

val make : int -> 'a -> 'a t
val get : 'a t -> int * int -> 'a
val set : 'a t -> int * int -> 'a -> unit
val get_subarray : 'a t -> int -> 'a Subarray.t
