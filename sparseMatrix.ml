type 'a t = { values: 'a array; counts: int array; indexes: int array }

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
