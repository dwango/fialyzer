open Base
open Obeam
open Ast

val expr_of_erlang_expr : Abstract_format.expr_t -> expr

val code_to_module : Abstract_format.t -> (module_, exn) Result.t

val code_to_expr : Abstract_format.t -> (expr, exn) Result.t
