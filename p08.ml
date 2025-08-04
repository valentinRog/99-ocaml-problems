let pack l =
  let rec pack_next = function
    | e1 :: (e2 :: _ as tl) when e1 != e2 -> ([ e1 ], tl)
    | e1 :: (e2 :: _ as tl) when e1 == e2 ->
        let acc, tl = pack_next tl in
        (e1 :: acc, tl)
    | l -> (l, [])
  in
  let rec f = function
    | [] -> []
    | l -> (
        match pack_next l with res, [] -> [ res ] | res, tl -> res :: f tl)
  in
  f l

let () =
  assert (pack [ 1; 1; 2; 3; 3; 3 ] = [ [ 1; 1 ]; [ 2 ]; [ 3; 3; 3 ] ]);
  assert (pack [ 1; 1; 1 ] = [ [ 1; 1; 1 ] ]);
  assert (pack [] = [])
