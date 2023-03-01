exception ExceptionOne
exception ExceptionTwo of string

let save_div x y =
  try x / y with
  | Division_by_zero -> 0

let v = match List.hd [] with
| [] -> "empty"
| _ :: _ -> "nonempty"
| exception Failure s-> s

(*v evaluates to "hd", because it raises the exception Failure "hd"*)