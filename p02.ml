let rec last_two = function
  | [] | [ _ ] -> None
  | [ e1; e2 ] -> Some (e1, e2)
  | _ :: tl -> last_two tl

let () =
  assert (last_two [ 1; 2; 3; 4 ] = Some (3, 4));
  assert (last_two [ 1 ] = None);
  assert (last_two [] = None)
