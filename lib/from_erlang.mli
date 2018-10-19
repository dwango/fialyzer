open Base
open Obeam

val expr_of_erlang_expr : Abstract_format.expr_t -> Ast_intf.expr

val code_to_module : Abstract_format.t -> (Ast_intf.module_, exn) Result.t

val code_to_expr : Abstract_format.t -> (Ast_intf.expr, exn) Result.t
