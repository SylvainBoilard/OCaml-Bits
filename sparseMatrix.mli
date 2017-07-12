type 'a t

val of_list : ((int * int) * 'a) list -> 'a t
val get : 'a t -> int * int -> 'a
val set : 'a t -> int * int -> 'a -> unit
