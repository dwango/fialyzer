open Ast_intf

type sol

val string_of_sol : sol -> string
         
val init : sol

val solve : sol -> constraint_ -> (sol, exn) result
