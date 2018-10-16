open Base
open Obeam

val expr_of_erlang_expr : Abstract_format.expr_t -> Types.expr

val code_to_module : Abstract_format.t -> (Types.module_, exn) Result.t

val code_to_expr : Abstract_format.t -> (Types.expr, exn) Result.t
