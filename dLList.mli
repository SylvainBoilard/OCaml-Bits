type 'a root
type 'a node

val create : unit -> 'a root
val first : 'a root -> 'a node
val first_opt : 'a root -> 'a node option
val last : 'a root -> 'a node
val last_opt : 'a root -> 'a node option
val next : 'a node -> 'a node
val next_opt : 'a node -> 'a node option
val prev : 'a node -> 'a node
val prev_opt : 'a node -> 'a node option
val add_first : 'a root -> 'a -> unit
val add_last : 'a root -> 'a -> unit
val insert_after : 'a node -> 'a -> unit
val insert_before : 'a node -> 'a -> unit
val is_empty : 'a root -> bool
val length : 'a root -> int
val copy : 'a root -> 'a root
val retrieve_root : 'a node -> 'a root
val remove : 'a node -> unit
val put_back : 'a node -> unit
val remove_and_neuter : 'a node -> unit
val get : 'a node -> 'a
val iter : ('a -> unit) -> 'a root -> unit
val rev_iter : ('a -> unit) -> 'a root -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b root -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a root -> 'b -> 'b
