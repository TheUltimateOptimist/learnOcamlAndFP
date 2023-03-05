let rec add1 = function
  | [] -> []
  | h :: t -> (h + 1) :: add1 t

(*adds "!" to each element of list*)
let rec concat_bang = function
  | [] -> []
  | h :: t -> (h ^ "!") :: concat_bang t


let rec map f = function
  | [] -> []
  | h :: t -> f h :: map f t

let rec correct_map f = function
  | [] -> []
  | h :: t -> let h' = f h in h' :: map f t

let add1 = map (fun x -> x + 1)
let concat_bang = map (fun x -> x ^ "!")

module TestModule = struct
  let lol = "lol"
end

