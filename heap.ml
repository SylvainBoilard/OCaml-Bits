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
    type h = H of elt * h list
    type t = h option

    exception Empty

    let empty = None

    let rec meld l' l =
      match l', l with
      | [], h :: [] | h :: [], [] -> h
      | _, h :: [] -> meld [] (h :: l')
      | _, [] -> assert (l' <> []); meld [] l'
      | _, (H (e1, l1) as h1) :: (H (e2, l2) as h2) :: tl ->
         if Ord.compare e1 e2 < 0 then
           meld (H (e1, h2 :: l1) :: l') tl
         else
           meld (H (e2, h1 :: l2) :: l') tl

    let merge h1 h2 =
      match h1, h2 with
      | Some (H (e1, l1) as h1), Some (H (e2, l2) as h2) ->
         if Ord.compare e1 e2 < 0 then
           Some (H (e1, h2 :: l1))
         else
           Some (H (e2, h1 :: l2))
      | None, h | h, None -> h
              
    let insert e = function
      | None -> Some (H (e, []))
      | Some (H (e', l)) when Ord.compare e' e < 0 ->
         Some (H (e', H (e, []) :: l))
      | Some h -> Some (H (e, [h]))

    let pop = function
      | None -> raise Empty
      | Some (H (_, [])) -> None
      | Some (H (_, l)) -> Some (meld [] l)

    let top = function
      | None -> raise Empty
      | Some (H (e, _)) -> e
  end
