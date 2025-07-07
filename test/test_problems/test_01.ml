open Ocaml_99_problems

let test () =
  assert (Problem_01.last [ 1; 2; 3 ] = Some 3);
  assert (Problem_01.last [ 1 ] = Some 1);
  assert (Problem_01.last [] = None)
