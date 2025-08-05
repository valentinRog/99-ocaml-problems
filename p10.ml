type 'a rle = One of 'a | Many of int * 'a

let encode l =
  let rec f l acc =
    match (l, acc) with
    | hd :: tl, [] -> f tl [ One hd ]
    | hd :: tl, One e :: acc_tl when hd = e -> f tl (Many (2, e) :: acc_tl)
    | hd :: tl, Many (n, e) :: acc_tl when hd = e ->
        f tl (Many (n + 1, e) :: acc_tl)
    | hd :: tl, acc_hd :: acc_tl -> f tl (One hd :: acc_hd :: acc_tl)
    | _ -> acc
  in
  f l [] |> List.rev

let () =
  assert (encode [ 1; 1; 2; 3; 3; 3 ] = [ Many (2, 1); One 2; Many (3, 3) ]);
  assert (encode [ 1; 1; 1 ] = [ Many (3, 1) ]);
  assert (encode [] = [])
