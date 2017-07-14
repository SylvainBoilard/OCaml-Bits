type 'a t = { values: 'a array; counts: int array; indexes: int array }
type 'a finger = { matrix: 'a t; index: int; row: int }

let of_list indexed_elements =
  let rev_sorted =
    List.sort (fun ((c1, r1), _) ((c2, r2), _) ->
        if r1 = r2 then c2 - c1 else r2 - r1
      ) indexed_elements
  in
  let max_row = ref 0 in
  let columns_indexes, row_indexes, elements =
    List.fold_left (fun (cl, rl, el) ((c, r), e) ->
        if c < 0 || r < 0 then invalid_arg "negative index";
        max_row := max r !max_row;
        c::cl, r::rl, e::el
      ) ([], [], []) rev_sorted
  in
  let matrix = {
      values = Array.of_list elements;
      counts = Array.make (!max_row + 2) 0;
      indexes = Array.of_list columns_indexes
    }
  in
  List.fold_left (fun p r ->
      for i = succ p to r do
        matrix.counts.(succ i) <- matrix.counts.(i)
      done;
      let o = succ r in
      matrix.counts.(o) <- succ matrix.counts.(o);
      r
    ) 0 row_indexes |> ignore;
  matrix

let flatten_coords matrix (col, row) =
  let rec loop start finish =
    match (start + finish) / 2 with
    | _ when start = finish -> raise Not_found
    | i when matrix.indexes.(i) = col -> i
    | i when matrix.indexes.(i) > col -> loop start i
    | i (* matrix.indexes.(i) < col *) -> loop (succ i) finish
  in
  try loop matrix.counts.(row) matrix.counts.(succ row)
  with Invalid_argument _ -> raise Not_found

let get matrix coords =
  matrix.values.(flatten_coords matrix coords)

let set matrix coords value =
  matrix.values.(flatten_coords matrix coords) <- value

let iter f matrix = Array.iter f matrix.values

let iteri f matrix =
  let row = ref 0 in
  Array.iteri (fun i v ->
      while matrix.counts.(succ !row) = i do incr row done;
      f (matrix.indexes.(i), !row) v
    ) matrix.values

let iterf f matrix =
  let row = ref 0 in
  Array.iteri (fun index v ->
      while matrix.counts.(succ !row) = index do incr row done;
      f { matrix; index; row = !row } v
    ) matrix.values

let finger matrix ((_, row) as coords) =
  { matrix; index = flatten_coords matrix coords; row }

let coords finger =
  finger.matrix.indexes.(finger.index), finger.row

let get_finger finger =
  finger.matrix.values.(finger.index)

let set_finger finger value =
  finger.matrix.values.(finger.index) <- value

let next_on_row finger =
  let matrix = finger.matrix in
  let next_index = succ finger.index in
  if next_index = matrix.counts.(succ finger.row) then
    raise Not_found
  else
    matrix.values.(next_index), matrix.indexes.(next_index)

let previous_on_row finger =
  let matrix = finger.matrix in
  let prev_index = pred finger.index in
  if prev_index < matrix.counts.(finger.row) then
    raise Not_found
  else
    matrix.values.(prev_index), matrix.indexes.(prev_index)

let next_on_column finger =
  let matrix = finger.matrix in
  let col = matrix.indexes.(finger.index) in
  let row_count = pred (Array.length matrix.counts) in
  let rec loop = function
    | n when n = row_count -> raise Not_found
    | n ->
       try matrix.values.(flatten_coords matrix (col, n)), n
       with Not_found -> loop (succ n)
  in
  loop finger.row

let previous_on_column finger =
  let matrix = finger.matrix in
  let col = matrix.indexes.(finger.index) in
  let rec loop = function
    | -1 -> raise Not_found
    | n ->
       try matrix.values.(flatten_coords matrix (col, n)), n
       with Not_found -> loop (pred n)
  in
  loop finger.row
