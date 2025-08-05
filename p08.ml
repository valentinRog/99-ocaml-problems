let pack l =
  let rec f l acc =
    match (l, acc) with
    | hd :: tl, [] -> f tl [ [ hd ] ]
    | hd :: tl, (e :: _ as acc_hd) :: acc_tl when hd = e ->
        f tl ((e :: acc_hd) :: acc_tl)
    | hd :: tl, acc_hd :: acc_tl -> f tl ([ hd ] :: acc_hd :: acc_tl)
    | _ -> acc
  in
  f l [] |> List.rev

let () =
  assert (pack [ 1; 1; 2; 3; 3; 3 ] = [ [ 1; 1 ]; [ 2 ]; [ 3; 3; 3 ] ]);
  assert (pack [ 1; 1; 1 ] = [ [ 1; 1; 1 ] ]);
  assert (pack [] = [])
