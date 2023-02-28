exception ExceptionOne
exception ExceptionTwo of string

let save_div x y =
  try x / y with
  | Division_by_zero -> 0