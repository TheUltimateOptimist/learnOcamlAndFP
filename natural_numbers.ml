(*nat is short for natural number*)
type nat = Zero | Succ of nat

let zero = Zero
let one = Succ zero
let two = Succ one
let three = Succ two
let four = Succ three

let rec iszero = function
  | Zero -> true
  | Succ _ -> false

let rec add n1 n2 = 
  match n1 with
   | Zero -> n2
   | Succ value -> add value (Succ n2)

let rec int_of_nat = function
  | Zero -> 0
  | Succ m -> 1 + int_of_nat m

let rec nat_of_int = function
  | i when i = 0 -> Zero
  | i when i > 0 -> Succ (nat_of_int (i - 1))
  | _ -> failwith "nat_of_int is undefined on negative ints"

let rec even = function Zero -> true | Succ m -> odd m
and odd = function Zero -> false | Succ m -> even m