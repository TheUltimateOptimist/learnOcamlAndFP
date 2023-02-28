(*Basically Ocaml default list implementation*)
type 'a mylist = 
  | []
  | (::) of 'a * 'a mylist

let rec length = function
 | [] -> 0
 | _ :: t -> 1 + length(t)



