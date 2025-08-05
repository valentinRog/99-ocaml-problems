let encode l =
  let rec f l acc =
    match (l, acc) with
    | hd :: tl, [] -> f tl [ (1, hd) ]
    | hd :: tl, (n, e) :: acc_tl when hd = e -> f tl ((n + 1, e) :: acc_tl)
    | hd :: tl, acc_hd :: acc_tl -> f tl ((1, hd) :: acc_hd :: acc_tl)
    | _ -> acc
  in
  f l [] |> List.rev

let () =
  assert (encode [ 1; 1; 2; 3; 3; 3 ] = [ (2, 1); (1, 2); (3, 3) ]);
  assert (encode [ 1; 1; 1 ] = [ (3, 1) ]);
  assert (encode [] = [])
