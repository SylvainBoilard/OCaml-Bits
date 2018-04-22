type ('a, _) elem =
  | Root : ('a, _) elem * ('a, _) elem -> ('a, [`root]) elem
  | Node : ('a, _) elem * ('a, _) elem * 'a -> ('a, [`node]) elem
type 'a root = ('a, [`root]) elem
type 'a node = ('a, [`node]) elem

type 'a mutable_elem = {
    mutable prev: 'a mutable_elem;
    mutable next: 'a mutable_elem
  }

let create () =
  (* We can not allocate a Root directly like this:

     let rec root = Root (root, root)

     because it might get optimized out as a compile-time constant
     since it is not supposed to have mutable fields. *)
  let rec root = { prev = root; next = root } in
  (Obj.magic root : 'a root)

let insert_between_elems prev next value =
  let node : 'a mutable_elem = Obj.magic (Node (prev, next, value)) in
  let prev : 'a mutable_elem = Obj.magic prev in
  let next : 'a mutable_elem = Obj.magic next in
  prev.next <- node;
  next.prev <- node

let remove (node : 'a node) =
  let node : 'a mutable_elem = Obj.magic node in
  if node.prev.next != node || node.next.prev != node
  then invalid_arg "DLList.remove: node already removed"
  else (node.prev.next <- node.next; node.next.prev <- node.prev)

let put_back (node : 'a node) =
  let node : 'a mutable_elem = Obj.magic node in
  if node.prev.next != node.next || node.next.prev != node.prev
  then invalid_arg "DLList.put_back: previous neighbors have invalid state"
  else (node.prev.next <- node; node.next.prev <- node)

let first = function
  | Root (_, (Node _ as node)) -> (node : 'a node)
  | Root (_, Root _) -> raise Not_found

let first_opt = function
  | Root (_, (Node _ as node)) -> Some (node : 'a node)
  | Root (_, Root _) -> None

let last = function
  | Root (Node _ as node, _) -> (node : 'a node)
  | Root (Root _, _) -> raise Not_found

let last_opt = function
  | Root (Node _ as node, _) -> Some (node : 'a node)
  | Root (Root _, _) -> None

let next = function
  | Node (_, (Node _ as node), _) -> (node : 'a node)
  | Node (_, Root _, _) -> raise Not_found

let next_opt = function
  | Node (_, (Node _ as node), _) -> Some (node : 'a node)
  | Node (_, Root _, _) -> None

let prev = function
  | Node (Node _ as node, _, _) -> (node : 'a node)
  | Node (Root _, _, _) -> raise Not_found

let prev_opt = function
  | Node (Node _ as node, _, _) -> Some (node : 'a node)
  | Node (Root _, _, _) -> None

let rec skip_aux : type k. int -> ('a, k) elem -> 'a node = fun n ->
  function
  | Root _ -> raise Not_found
  | Node _ as node when n = 0 -> node
  | Node (_, next, _) -> skip_aux (pred n) next

let rec rev_skip_aux : type k. int -> ('a, k) elem -> 'a node = fun n ->
  function
  | Root _ -> raise Not_found
  | Node _ as node when n = 0 -> node
  | Node (prev, _, _) -> rev_skip_aux (pred n) prev

let at index root =
  let Root (_, first) = root in
  if index < 0
  then invalid_arg "DLList.at: negative index"
  else skip_aux index first

let rev_at index root =
  let Root (last, _) = root in
  if index < 0
  then invalid_arg "DLList.rev_at: negative index"
  else rev_skip_aux index last

let skip count (node : 'a node) =
  if count < 0
  then invalid_arg "DLList.skip: negative count"
  else skip_aux count node

let rev_skip count (node : 'a node) =
  if count < 0
  then invalid_arg "DLList.rev_skip: negative count"
  else rev_skip_aux count node

let add_first root value =
  let Root (_, first) = root in
  insert_between_elems root first value

let add_last root value =
  let Root (last, _) = root in
  insert_between_elems last root value

let insert_after node value =
  let Node (_, next, _) = node in
  insert_between_elems node next value

let insert_before node value =
  let Node (prev, _, _) = node in
  insert_between_elems prev node value

let is_empty = function
  | Root (Node _, _) -> false
  | Root (Root _, _) -> true

let length root =
  let rec aux : type k. int -> ('a, k) elem -> int = fun count ->
    function
    | Node (_, next, _) -> aux (succ count) next
    | Root _ -> count
  in
  let Root (_, first) = root in
  aux 0 first

let copy dllist =
  let new_root = create () in
  let rec aux : type k. ('a, k) elem -> 'a root =
    function
    | Node (_, next, v) ->
       let Root (new_last, _) = new_root in
       insert_between_elems new_last new_root v;
       aux next
    | Root _ -> new_root
  in
  let Root (_, first) = dllist in
  aux first

let rec retrieve_root : type k. ('a, k) elem -> 'a root = function
  | Node (_, next, _) -> retrieve_root next
  | Root _ as root -> root

let get (Node (_, _, v)) = v

let iter f root =
  let rec aux : type k. ('a, k) elem -> unit = function
    | Node (_, next, v) -> f v; aux next
    | Root _ -> ()
  in
  let Root (_, first) = root in
  aux first

let rev_iter f root =
  let rec aux : type k. ('a, k) elem -> unit = function
    | Node (prev, _, v) -> f v; aux prev
    | Root _ -> ()
  in
  let Root (last, _) = root in
  aux last

let fold_left f a root =
  let rec aux : type k. 'b -> ('a, k) elem -> 'b = fun acc ->
    function
    | Node (_, next, v) -> aux (f acc v) next
    | Root _ -> acc
  in
  let Root (_, first) = root in
  aux a first

let fold_right f root a =
  let rec aux : type k. 'b -> ('a, k) elem -> 'b = fun acc ->
    function
    | Node (prev, _, v) -> aux (f v acc) prev
    | Root _ -> acc
  in
  let Root (last, _) = root in
  aux a last

let of_list list =
  let root = create () in
  let rec aux = function
    | [] -> root
    | hd :: tl ->
       let Root (prev, _) = root in
       insert_between_elems prev root hd;
       aux tl
  in
  aux list
