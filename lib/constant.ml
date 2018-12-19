open Base
module Format = Caml.Format

type constant =
    | Number of int
    | Atom of string
[@@deriving show, sexp_of]

let pp = function
  | Number n -> Int.to_string n
  | Atom a -> "'" ^ a ^ "'"
