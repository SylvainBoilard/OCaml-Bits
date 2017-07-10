type 'a t = {
    length: int;
    data: 'a array
  }

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

let unsafe_get a c =
  let i, j = norm_coords c in
  Array.unsafe_get a.data (i + j * (j + 1) / 2)

let unsafe_set a c e =
  let i, j = norm_coords c in
  Array.unsafe_set a.data (i + j * (j + 1) / 2) e
