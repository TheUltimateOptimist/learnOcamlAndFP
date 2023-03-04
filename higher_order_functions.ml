let double x = 2 * x
let square x = x * x

let twice f x = f (f x)

let quad x = twice double x
let fourth x = twice square x

let pipeline x f = f x
let (|>) = pipeline
let x = 5 |> double |> square

let compose f g x = f (g x)

let square_then_double = compose double square
let x = square_then_double 1
let y = square_then_double 2

let both f g x = (f x, g x)
let ds = both double square
let p = ds 3

(*a function that conditionally chooses which of two functions to apply based on a predicate*)
let cond p f g x = if p x then f x else g x
