open Base
module Format = Caml.Format

type constant =
    | Number of int
    | Atom of string
[@@deriving show, sexp_of]
