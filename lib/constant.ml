open Base

type t =
    | Number of int
    | Atom of string
[@@deriving sexp_of]

let pp = function
  | Number n -> Int.to_string n
  | Atom a -> "'" ^ a ^ "'"
