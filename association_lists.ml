let d = [("rectangle", 4); ("nonagon", 9); ("icosagon", 20)]

let insert k v lst = (k, v) :: lst

let rec lookup key = function
  | [] -> None
  | (key', value) :: tail -> if key = key' then Some value else lookup key tail

(*
The insert function simply adds a new map from a key to a value at the front of the list. It doesn’t bother to check whether the key is already in the list. The lookup function looks through the list from left to right. So if there did happen to be multiple maps for a given key in the list, only the most recently inserted one would be returned.

Insertion in an association list is therefore constant time, and lookup is linear time. Although there are certainly more efficient implementations of dictionaries—and we’ll study some later in this course—association lists are a very easy and useful implementation for small dictionaries that aren’t performance critical. The OCaml standard library has functions for association lists in the List module; look for List.assoc and the functions below it in the documentation. What we just wrote as lookup is actually already defined as List.assoc_opt. There is no pre-defined insert function in the library because it’s so trivial just to cons a pair on.   
*)



