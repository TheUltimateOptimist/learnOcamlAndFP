type primary_color = Red | Green | Blue

let r = Red

type point = float * float

type shape = 
  | Circle of {center: point; radius: float}
  | Rectangle of {lower_left: point; upper_right: point}
  | Point of point

let c1 = Circle{center=(0., 0.); radius=1.}
let rec1 = Rectangle{lower_left=(-1., -1.); upper_right=(1., 1.)}
let p1 = Point (31., 10.)
let avg a b = (a +. b) /. 2.

let area = function
  | Point _ -> 0.0
  | Circle {center = _; radius} -> Float.pi *. (radius ** 2.0)
  | Rectangle {lower_left=(x1, y1); upper_right=(x2, y2)} ->
      let w = x2 -. x1 in
      let h = y2 -. y1 in
      w *. h

let center shape = 
  match shape with
  | Circle{center; _} -> center
  | Rectangle{lower_left; upper_right} -> 
    let (x_ll, y_ll) = lower_left in 
    let (x_ur, y_ur) = upper_right in
    (avg x_ll x_ur, avg y_ll y_ur)
  | Point center -> center


(*nested pattern matching*)
let center_nested shape = 
  match shape with
  | Circle{center; _} -> center
  | Rectangle{lower_left = (x_ll, y_ll); upper_right = (x_ur, y_ur)} -> 
    (avg x_ll x_ur, avg y_ll y_ur)
  | Point center -> center


type string_or_int = String of string | Int of int
let rec sum : string_or_int list -> int = function
  | [] -> 0
  | String s :: t -> int_of_string s + sum t
  | Int i :: t -> i + sum t

let lst_sum = sum [String "1"; Int 2]

(*Variants also make it possible to discriminate
  which tag a value was constructed with, even if
  multiple constructors carry the same type.
  For example:
*)
type t = Left of int | Right of int
let x = Left 1
let double_right = function
  | Left i -> i
  | Right i -> 2 * i

  



