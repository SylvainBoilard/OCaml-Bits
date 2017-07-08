let solve mm =
  let m = Array.map Array.copy mm in
  let len = Array.length m in
  assert (len <> 0 && Array.length m.(0) = len + 1);
  (* Gauss pivot *)
  (* Column per column: *)
  for i = 0 to len - 1 do
    (* Find the pivot amongst the remaining lines. *)
    let pr = ref (abs_float m.(i).(i), i) in
    for j = i + 1 to len - 1 do
      let v = abs_float m.(j).(i) in
      if v > fst !pr then pr := v, j
    done;
    (* Move the pivot line to its place. *)
    let p = snd !pr in
    let t = m.(p) in
    m.(p) <- m.(i);
    m.(i) <- t;
    (* Recompute the pivot line. *)
    let d = m.(i).(i) in
    m.(i).(i) <- 1.;
    for j = i + 1 to len do
      m.(i).(j) <- m.(i).(j) /. d
    done;
    (* Recompute the other lines. *)
    for j = 0 to len - 1 do
      if j <> i then
        begin
          let v = m.(j).(i) in
          for k = i to len do
            m.(j).(k) <- m.(j).(k) -. m.(i).(k) *. v
          done
        end
    done
  done;
  Array.map (fun a -> a.(len)) m
