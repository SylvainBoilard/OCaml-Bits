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
    val singleton: elt -> t
    val insert: elt -> t -> t
    val merge: t -> t -> t
    val pop: t -> t
    val top: t -> elt
  end

module Make (Ord: OrderedType) =
  struct
    type elt = Ord.t
    type t = EmptyHeap | Heap of elt * t list

    exception Empty

    let empty = EmptyHeap

    let singleton e = Heap (e, [])

    let insert e = function
      | EmptyHeap -> Heap (e, [])
      | Heap (e', l) when Ord.compare e' e < 0 ->
         Heap (e', Heap (e, []) :: l)
      | h -> Heap (e, [h])

    let merge h1 h2 = match h1, h2 with
      | EmptyHeap, h | h, EmptyHeap -> h
      | Heap (e1, l1), Heap (e2, l2) ->
         if Ord.compare e1 e2 < 0
         then Heap (e1, h2 :: l1)
         else Heap (e2, h1 :: l2)

    let meld l =
      let rec aux acc = function
        | [] -> acc
        | h :: [] -> merge acc h
        | h1 :: h2 :: tl -> aux (merge acc (merge h1 h2)) tl
      in
      aux EmptyHeap l

    let pop = function
      | EmptyHeap -> raise Empty
      | Heap (_, l) -> meld l

    let top = function
      | EmptyHeap -> raise Empty
      | Heap (e, _) -> e
  end
