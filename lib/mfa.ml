(**
   The tuple of module_name, function_name and arity
 *)
open Base
module Format = Caml.Format

type t = string * string * int [@@deriving show, sexp_of]
