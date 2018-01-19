type 'a root = 'a neighbors
 and 'a node = 'a neighbors * 'a
 and 'a neighbors = {
     mutable prev: 'a elem;
     mutable next: 'a elem
   }
 and 'a elem =
   | Root of 'a root
   | Node of 'a node

let poly_self elem = match elem.next with
  | Node ({ prev; _ }, _) | Root { prev; _ } -> prev

let create () =
  let rec dllist = { prev = Root dllist; next = Obj.magic () } in
  dllist.next <- dllist.prev; (* Only one elem should exist per element. *)
  dllist

let first = function
  | { next = Node node; _ } -> node
  | _ -> raise Not_found

let first_opt = function
  | { next = Node node; _ } -> Some node
  | _ -> None

let last = function
  | { prev = Node node; _ } -> node
  | _ -> raise Not_found

let last_opt = function
  | { prev = Node node; _ } -> Some node
  | _ -> None

let next = function
  | { next = Node node; _ }, _ -> node
  | _ -> raise Not_found

let next_opt = function
  | { next = Node node; _ }, _ -> Some node
  | _ -> None

let prev = function
  | { prev = Node node; _ }, _ -> node
  | _ -> raise Not_found

let prev_opt = function
  | { prev = Node node; _ }, _ -> Some node
  | _ -> None

let insert_before_neighbor elem value =
  let prev_elem = match elem.prev with Node (prev, _) | Root prev -> prev in
  let new_node = Node ({ prev = elem.prev; next = prev_elem.next }, value) in
  prev_elem.next <- new_node;
  elem.prev <- new_node

let insert_after_neighbor elem value =
  let next_elem = match elem.next with Node (next, _) | Root next -> next in
  let new_node = Node ({ prev = next_elem.prev; next = elem.next }, value) in
  next_elem.prev <- new_node;
  elem.next <- new_node

let add_first root value = insert_after_neighbor root value

let add_last root value = insert_before_neighbor root value

let insert_after (node, _) value = insert_after_neighbor node value

let insert_before (node, _) value = insert_before_neighbor node value

let is_empty = function
  | { next = Node _; _ } -> false
  | { next = Root _; _ } -> true

let length root =
  let rec loop acc = function
    | Node ({ next; _ }, _) -> loop (succ acc) next
    | Root _ -> acc
  in
  loop 0 root.next

let rec retrieve_root = function
  | { next = Node node; _ }, _ -> retrieve_root node
  | { next = Root root; _ }, _ -> root

let remove (node, _) =
  (match node.prev with Node (prev, _) | Root prev -> prev.next <- node.next);
  (match node.next with Node (next, _) | Root next -> next.prev <- node.prev)

let put_back ((node, _) as elem) =
  let self = Node elem in
  (match node.prev with Node (prev, _) | Root prev -> prev.next <- self);
  (match node.next with Node (next, _) | Root next -> next.prev <- self)

let remove_and_neuter ((node, _) as elem) =
  let poly_node = poly_self node in
  remove elem;
  node.next <- poly_node;
  node.prev <- poly_node

let get (_, v) = v

let iter f root =
  let rec loop = function
    | Node ({ next; _ }, v) -> f v; loop next
    | Root _ -> ()
  in
  loop root.next

let rev_iter f root =
  let rec loop = function
    | Node ({ prev; _ }, v) -> f v; loop prev
    | Root _ -> ()
  in
  loop root.prev

let fold_left f a root =
  let rec aux acc = function
    | Node ({ next; _ }, v) -> aux (f acc v) next
    | Root _ -> acc
  in
  aux a root.next
