open Base

val reset_tyvar_count : unit -> unit

val derive : Context.t -> Ast_intf.expr -> (Ast_intf.typ * Ast_intf.constraint_, string) Result.t
