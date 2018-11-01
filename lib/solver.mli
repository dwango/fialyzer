open Ast_intf

type solution

val string_of_sol : solution -> string

val init : solution

val solve : solution -> constraint_ -> (solution, exn) result
