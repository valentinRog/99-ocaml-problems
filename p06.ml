type 'a node = One of 'a | Many of 'a node list

let rec flatten (l : 'a node list) : 'a list =
  let rec f acc = function
    | [] -> acc
    | One x :: tl -> f (x :: acc) tl
    | Many ll :: tl -> f (f acc ll) tl
  in
  List.rev (f [] l)

let () =
  assert (
    flatten [ One 1; Many [ One 2; Many [ One 3; One 4 ]; One 5 ] ]
    = [ 1; 2; 3; 4; 5 ]);
  assert (flatten [] = [])
