let is_palindrome l = l = List.rev l

let () =
  assert (is_palindrome [ 1; 2; 3; 2; 1 ]);
  assert (is_palindrome [ 1 ]);
  assert (is_palindrome []);
  assert (not (is_palindrome [ 1; 2; 3 ]))
