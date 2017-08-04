type 'a t = {
    elements_by_row: 'a array;
    by_row_to_column: int array;
    by_column_to_row: int array;
    column_indexes: int array;
    row_indexes: int array;
    counts_to_row: int array
  }

module Finger =
  struct
    type 'a matrix = 'a t
    type 'a t = { matrix: 'a matrix; index: int }

    let coords finger =
      finger.matrix.column_indexes.(finger.index),
      finger.matrix.row_indexes.(finger.index)

    let get finger =
      finger.matrix.elements_by_row.(finger.index)

    let set finger value =
      finger.matrix.elements_by_row.(finger.index) <- value

    let next finger =
      if finger.index = pred (Array.length finger.matrix.elements_by_row) then
        raise Not_found
      else
        { finger with index = succ finger.index }

    let previous finger =
      if finger.index = 0 then
        raise Not_found
      else
        { finger with index = pred finger.index }

    let next_on_row finger =
      let row_indexes = finger.matrix.row_indexes in
      let new_index = succ finger.index in
      try
        if row_indexes.(finger.index) = row_indexes.(new_index) then
          { finger with index = new_index }
        else
          raise Not_found
      with Invalid_argument _ -> raise Not_found

    let previous_on_row finger =
      let row_indexes = finger.matrix.row_indexes in
      let new_index = pred finger.index in
      try
        if row_indexes.(finger.index) = row_indexes.(new_index) then
          { finger with index = new_index }
        else
          raise Not_found
      with Invalid_argument _ -> raise Not_found

    let next_on_column finger =
      let column_indexes = finger.matrix.column_indexes in
      let index_by_columns = finger.matrix.by_row_to_column.(finger.index) in
      try
        let new_index = finger.matrix.by_column_to_row.(succ index_by_columns) in
        if column_indexes.(finger.index) = column_indexes.(new_index) then
          { finger with index = new_index }
        else
          raise Not_found
      with Invalid_argument _ -> raise Not_found

    let previous_on_column finger =
      let column_indexes = finger.matrix.column_indexes in
      let index_by_columns = finger.matrix.by_row_to_column.(finger.index) in
      try
        let new_index = finger.matrix.by_column_to_row.(pred index_by_columns) in
        if column_indexes.(finger.index) = column_indexes.(new_index) then
          { finger with index = new_index }
        else
          raise Not_found
      with Invalid_argument _ -> raise Not_found

    let compare f1 f2 =
      if f1.matrix == f2.matrix then
        f1.index - f2.index
      else
        invalid_arg "Finger.compare: fingers from different matrices"
  end

let of_list indexed_elements =
  let module List =
    struct
      include List

      let yield l =
        let l = ref l in
        let aux _ =
          match !l with
          | hd::tl -> l := tl; hd
          | [] -> failwith "yield"
        in
        aux

      let rev_mapi f l =
        let rec aux acc n = function
          | [] -> acc
          | hd::tl -> aux (f n hd::acc) (succ n) tl
        in
        aux [] 0 l
    end
  in
  let rev_sorted_by_row =
    List.sort (fun ((c1, r1), _) ((c2, r2), _) ->
        if r1 = r2 then c2 - c1 else r2 - r1
      ) indexed_elements
  in
  let max_row = ref 0 in
  let element_count = ref 0 in
  let column_indexes, row_indexes, elements_by_row =
    List.fold_left (fun (cl, rl, el) ((c, r), e) ->
        if c < 0 || r < 0 then invalid_arg "negative index";
        max_row := max r !max_row;
        incr element_count;
        c::cl, r::rl, e::el
      ) ([], [], []) rev_sorted_by_row
  in
  let by_column_to_row =
    let revi = pred !element_count in
    List.rev_mapi (fun i (c, _) -> c, revi - i) rev_sorted_by_row
    |> List.sort (fun ((c1, r1), _) ((c2, r2), _) ->
           if c1 = c2 then r2 - r1 else c2 - c1
         ) |> List.rev_map snd
  in
  let matrix = {
      elements_by_row = Array.init !element_count (List.yield elements_by_row);
      by_row_to_column = Array.make !element_count 0;
      by_column_to_row =
        Array.init !element_count (List.yield by_column_to_row);
      column_indexes = Array.init !element_count (List.yield column_indexes);
      row_indexes = Array.init !element_count (List.yield row_indexes);
      counts_to_row = Array.make (!max_row + 2) 0
    }
  in
  List.iteri (fun i v -> matrix.by_row_to_column.(v) <- i) by_column_to_row;
  List.fold_left (fun p r ->
      for i = succ p to r do
        matrix.counts_to_row.(succ i) <- matrix.counts_to_row.(i)
      done;
      let o = succ r in
      matrix.counts_to_row.(o) <- succ matrix.counts_to_row.(o);
      r
    ) 0 row_indexes |> ignore;
  matrix

let flatten_coords matrix (column, row) =
  let rec loop start finish =
    match (start + finish) / 2 with
    | _ when start = finish -> raise Not_found
    | i when matrix.column_indexes.(i) = column -> i
    | i when matrix.column_indexes.(i) > column -> loop start i
    | i (* matrix.column_indexes.(i) < column *) -> loop (succ i) finish
  in
  try loop matrix.counts_to_row.(row) matrix.counts_to_row.(succ row)
  with Invalid_argument _ -> raise Not_found

let cardinal matrix = Array.length matrix.elements_by_row

let get matrix coords =
  matrix.elements_by_row.(flatten_coords matrix coords)

let set matrix coords value =
  matrix.elements_by_row.(flatten_coords matrix coords) <- value

let finger matrix coords =
  { Finger.matrix; index = flatten_coords matrix coords }

let first matrix =
  if matrix.elements_by_row = [||] then
    raise Not_found
  else
    { Finger.matrix; index = 0 }

let last matrix =
  if matrix.elements_by_row = [||] then
    raise Not_found
  else
    { Finger.matrix; index = pred (Array.length matrix.elements_by_row) }

let iter f matrix = Array.iter f matrix.elements_by_row

let iteri f matrix =
  let row = ref 0 in
  Array.iteri (fun i v ->
      while matrix.counts_to_row.(succ !row) = i do incr row done;
      f (matrix.column_indexes.(i), !row) v
    ) matrix.elements_by_row

let iterf f matrix =
  let row = ref 0 in
  Array.iteri (fun index v ->
      while matrix.counts_to_row.(succ !row) = index do incr row done;
      f { Finger.matrix; index } v
    ) matrix.elements_by_row

let fold f acc matrix =
  Array.fold_left f acc matrix.elements_by_row

let foldf f acc matrix =
  let last_finger = last matrix in
  let rec aux acc = function
    | finger when finger = last_finger -> f acc finger
    | finger -> aux (f acc finger) (Finger.next finger)
  in
  aux acc (first matrix)
