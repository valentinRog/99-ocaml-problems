let rec last l =
  match l with [ x ] -> Some x | _ :: tl -> last tl | [] -> None

let () =
  assert (last [ 1; 2; 3 ] = Some 3);
  assert (last [ 1 ] = Some 1);
  assert (last [] = None)
