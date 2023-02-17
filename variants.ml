type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
type second = Sun | Mon
let dayInt d =
  match d with
  | Tue -> 2
  | Wed -> 3
  | Thu -> 4
  | Fri -> 5
  | Sat -> 6
  | Sun -> 7
  | Mon -> 1