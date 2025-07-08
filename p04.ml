let rec rev l =
  let rec f acc l = match l with [] -> acc | hd :: tl -> f (hd :: acc) tl in
  f [] l

let () =
  assert (rev [ 1; 2; 3 ] = [ 3; 2; 1 ]);
  assert (rev [ 1 ] = [ 1 ]);
  assert (rev [] = [])
