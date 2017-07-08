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

    let rec meld acc = function
      | EmptyHeap::_ | _::EmptyHeap::_ -> assert false
      | h::[] when acc = [] -> h
      | [] -> assert (acc <> []); meld [] acc
      | h::[] -> meld [] (h::acc)
      | (Heap (e1, l1) as h1)::(Heap (e2, l2) as h2)::tl ->
         if Ord.compare e1 e2 < 0 then
           meld (Heap (e1, h2::l1)::acc) tl
         else
           meld (Heap (e2, h1::l2)::acc) tl
              
    let insert e = function
      | EmptyHeap -> Heap (e, [])
      | Heap (e', l) when Ord.compare e' e < 0 -> Heap (e', Heap (e, [])::l)
      | h -> Heap (e, [h])

    let pop = function
      | EmptyHeap -> raise Empty
      | Heap (_, []) -> EmptyHeap
      | Heap (_, l) -> meld [] l

    let top = function
      | EmptyHeap -> raise Empty
      | Heap (e, _) -> e
  end
