open Base
open Obeam

val expr_of_erlang_expr : Abstract_format.expr_t -> Ast.t

val code_to_module : Abstract_format.t -> (Ast.module_, exn) Result.t

val module_to_expr : Ast.module_ -> (Ast.t, exn) Result.t

val code_to_expr : Abstract_format.t -> (Ast.t, exn) Result.t

(* export for unit-test *)
val extract_match_expr : Abstract_format.expr_t -> Abstract_format.expr_t list
