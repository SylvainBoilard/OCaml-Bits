module Finger :
  sig
    type 'a t

    val coords : 'a t -> int * int
    val get : 'a t -> 'a
    val set : 'a t -> 'a -> unit
    val next : 'a t -> 'a t
    val previous : 'a t -> 'a t
    val next_on_row : 'a t -> 'a t
    val previous_on_row : 'a t -> 'a t
    val next_on_column : 'a t -> 'a t
    val previous_on_column : 'a t -> 'a t
  end

type 'a t

val of_list : ((int * int) * 'a) list -> 'a t
val get : 'a t -> int * int -> 'a
val set : 'a t -> int * int -> 'a -> unit
val finger : 'a t -> int * int -> 'a Finger.t
val first : 'a t -> 'a Finger.t
val last : 'a t -> 'a Finger.t
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int * int -> 'a -> unit) -> 'a t -> unit
val iterf : ('a Finger.t -> 'a -> unit) -> 'a t -> unit

