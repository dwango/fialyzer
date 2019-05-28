open Base

type number = Int of int | Float of float
[@@deriving sexp_of]

type t =
    | Number of number
    | Atom of string
[@@deriving sexp_of]

let pp = function
  | Number (Int i) -> Int.to_string i
  | Number (Float f) -> Float.to_string f
  | Atom a -> "'" ^ a ^ "'"
