type 'a t

val make : int -> 'a -> 'a t
val get : 'a t -> int * int -> 'a
val set : 'a t -> int * int -> 'a -> unit
