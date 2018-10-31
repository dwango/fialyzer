open Base
open Ast_intf

val derive : Context.t -> expr -> (typ * constraint_, string) Result.t
