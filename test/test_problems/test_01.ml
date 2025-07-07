open Ocaml_99_problems

let test () =
  assert (Problem_1.last [ 1; 2; 3 ] = Some 3);
  assert (Problem_1.last [ 1 ] = Some 1);
  assert (Problem_1.last [] = None)
