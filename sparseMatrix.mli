type 'a t
type 'a finger

val of_list : ((int * int) * 'a) list -> 'a t
val get : 'a t -> int * int -> 'a
val set : 'a t -> int * int -> 'a -> unit
val iter : ('a -> unit) -> 'a t -> unit
val iteri : (int * int -> 'a -> unit) -> 'a t -> unit
val finger : 'a t -> int * int -> 'a finger
val iterf : ('a finger -> 'a -> unit) -> 'a t -> unit
val coords : 'a finger -> int * int
val get_finger : 'a finger -> 'a
val set_finger : 'a finger -> 'a -> unit
val next_on_row : 'a finger -> 'a * int
val previous_on_row : 'a finger -> 'a * int
val next_on_column : 'a finger -> 'a * int
val previous_on_column : 'a finger -> 'a * int
