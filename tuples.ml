type time = int * int * string
type point = float * float
let t = (10, 10, "am")
let p = (5., 3.5)

(*pattern matching*)
let px = match p with (x, _) -> x
let py = match p with (_, y) -> y