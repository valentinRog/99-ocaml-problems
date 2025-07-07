let rec last l =
  match l with [ x ] -> Some x | _ :: tl -> last tl | [] -> None
