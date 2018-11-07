(**
   The tuple of module_name, function_name and arity
 *)
open Base

type t = string * string * int [@@deriving sexp_of]
