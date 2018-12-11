(**
   The tuple of module_name, function_name and arity
 *)
open Base
module Format = Caml.Format

type t = {module_name: string;  function_name: string; arity: int} [@@deriving show, sexp_of]
