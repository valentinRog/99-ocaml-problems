let rec compress = function
  | e1 :: (e2 :: _ as tl) when e1 != e2 -> e1 :: compress tl
  | e1 :: (e2 :: _ as tl) -> compress tl
  | l -> l

let () =
  assert (compress [ 1; 2; 2; 2; 3; 3 ] = [ 1; 2; 3 ]);
  assert (compress [ 1; 2; 2; 1; 2; 3; 3 ] = [ 1; 2; 1; 2; 3 ]);
  assert (compress [ 1 ] = [ 1 ]);
  assert (compress [ 1; 1 ] = [ 1 ]);
  assert (compress [] = [])
