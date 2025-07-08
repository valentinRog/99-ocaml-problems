let rec at i l =
  match (i, l) with
  | i, _ when i < 0 -> None
  | _, [] -> None
  | 0, hd :: _ -> Some hd
  | _, _ :: tl -> at (i - 1) tl

let () =
  assert (at 2 [ 1; 2; 3; 4 ] = Some 3);
  assert (at 3 [ 1; 2; 3; 4 ] = Some 4);
  assert (at 0 [ 1 ] = Some 1);
  assert (at 0 [] = None);
  assert (at 3 [ 1; 2 ] = None)
