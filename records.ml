type student = {
  name : string;
  year : int;
}

let studentOne = {
  name = "Ruth Bader";
  year = 1954;
}

let studentTwo = {studentOne with name = "Second Name"}

(*pattern matching with record*)
let copyStudent = function {name; year;} -> {name = name; year = year}
let copy_student = function {name = name; year = year;} -> {name = name; year = year}