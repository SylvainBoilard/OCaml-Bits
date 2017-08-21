module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    exception Empty
    val empty: t
    val merge: t -> t -> t
    val insert: elt -> t -> t
    val pop: t -> t
    val top: t -> elt
  end

module Make (Ord: OrderedType) =
  struct
    type elt = Ord.t
    type t = EmptyHeap | Heap of elt * t list

    exception Empty

    let empty = EmptyHeap

    let rec meld l' l =
      match l', l with
      | _, EmptyHeap :: _ | _, _ :: EmptyHeap :: _
      | EmptyHeap :: _, _ -> assert false
      | [], h :: [] | h :: [], [] -> h
      | _, h :: [] -> meld [] (h :: l')
      | _, [] -> assert (l' <> []); meld [] l'
      | _, (Heap (e1, l1) as h1) :: (Heap (e2, l2) as h2) :: tl ->
         if Ord.compare e1 e2 < 0 then
           meld (Heap (e1, h2 :: l1) :: l') tl
         else
           meld (Heap (e2, h1 :: l2) :: l') tl

    let merge h1 h2 =
      match h1, h2 with
      | Heap (e1, l1), Heap (e2, l2) ->
         if Ord.compare e1 e2 < 0 then
           Heap (e1, h2 :: l1)
         else
           Heap (e2, h1 :: l2)
      | EmptyHeap, h | h, EmptyHeap -> h
              
    let insert e = function
      | EmptyHeap -> Heap (e, [])
      | Heap (e', l) when Ord.compare e' e < 0 ->
         Heap (e', Heap (e, []) :: l)
      | h -> Heap (e, [h])

    let pop = function
      | EmptyHeap -> raise Empty
      | Heap (_, []) -> EmptyHeap
      | Heap (_, l) -> meld [] l

    let top = function
      | EmptyHeap -> raise Empty
      | Heap (e, _) -> e
  end
