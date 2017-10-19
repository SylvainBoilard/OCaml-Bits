type nil
type value

type ('a, _) data =
  | Nil : ('a, nil) data
  | Value : 'a -> ('a, value) data

type ('a, 't) elem = {
    mutable prev: 'a poly_elem;
    mutable next: 'a poly_elem;
    data: ('a, 't) data
  }
 and 'a poly_elem =
   | Root of ('a, nil) elem
   | Node of ('a, value) elem

type 'a root = ('a, nil) elem
type 'a node = ('a, value) elem

let poly_self elem = match elem.next with
  | Node { prev; _ } -> prev
  | Root { prev; _ } -> prev

let create () =
  let rec dllist = { prev = Root dllist; next = Obj.magic (); data = Nil } in
  dllist.next <- dllist.prev; (* Only one poly_elem must exist per elem. *)
  dllist

let first = function
  | { data = Nil; next = Node node; _ } -> node
  | _ -> raise Not_found

let first_opt = function
  | { data = Nil; next = Node node; _ } -> Some node
  | _ -> None

let last = function
  | { data = Nil; prev = Node node; _ } -> node
  | _ -> raise Not_found

let last_opt = function
  | { data = Nil; prev = Node node; _ } -> Some node
  | _ -> None

let next = function
  | { data = Value _; next = Node node; _ } -> node
  | _ -> raise Not_found

let next_opt = function
  | { data = Value _; next = Node node; _ } -> Some node
  | _ -> None

let prev = function
  | { data = Value _; prev = Node node; _ } -> node
  | _ -> raise Not_found

let prev_opt = function
  | { data = Value _; prev = Node node; _ } -> Some node
  | _ -> None

let poly_insert_before elem value =
  let aux prev_elem =
    let new_node =
      Node { prev = elem.prev; next = prev_elem.next; data = Value value } in
    prev_elem.next <- new_node;
    elem.prev <- new_node
  in
  match elem.prev with Node node -> aux node | Root root -> aux root

let poly_insert_after elem value =
  let aux next_elem =
    let new_node =
      Node { prev = next_elem.prev; next = elem.next; data = Value value } in
    next_elem.prev <- new_node;
    elem.next <- new_node
  in
  match elem.next with Node node -> aux node | Root root -> aux root

let insert_before (node : 'a node) value =
  poly_insert_before node value

let insert_after (node : 'a node) value =
  poly_insert_after node value

let add_first (root : 'a root) value =
  poly_insert_after root value

let add_last (root : 'a root) value =
  poly_insert_before root value

let remove (node : 'a node) =
  let aux_prev prev_elem = prev_elem.next <- node.next in
  let aux_next next_elem = next_elem.prev <- node.prev in
  let poly_node = poly_self node in
  (match node.prev with Root r -> aux_prev r | Node n -> aux_prev n);
  (match node.next with Root r -> aux_next r | Node n -> aux_next n);
  node.prev <- poly_node;
  node.next <- poly_node

let get { data = Value v; _ } = v
