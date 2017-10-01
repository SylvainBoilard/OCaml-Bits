type 'a t = {
    length: int;
    data: 'a array
  }

module Subarray =
  struct
    type 'a symarray = 'a t
    type 'a t = { symarray: 'a symarray; index: int }

    let fold_left f acc sa =
      let rec aux acc index i =
        if i < sa.index then
          aux (f acc sa.symarray.data.(index)) (succ index) (succ i)
        else if i < sa.symarray.length then
          aux (f acc sa.symarray.data.(index)) (succ index + i) (succ i)
        else acc
      in
      aux acc (sa.index * (sa.index + 1) / 2) 0
  end

let norm_coords ((i, j) as c) = if i <= j then c else j, i

let make l e =
  try { length = l; data = Array.make (l * (l + 1) / 2) e }
  with Invalid_argument _ -> invalid_arg "SymetricArray.make"

let get a c =
  let i, j = norm_coords c in
  if i < 0 || j >= a.length then
    invalid_arg "index out of bounds"
  else
    Array.unsafe_get a.data (i + j * (j + 1) / 2)

let set a c e =
  let i, j = norm_coords c in
  if i < 0 || j >= a.length then
    invalid_arg "index out of bounds"
  else
    Array.unsafe_set a.data (i + j * (j + 1) / 2) e

let get_subarray symarray index = Subarray.{ symarray; index }
