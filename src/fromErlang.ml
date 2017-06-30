open Types
open Obeam

module F = Abstract_format
         
let unit : expr = Val (Int 0)

let const_of_literal = function
  | F.LitAtom (_line_t, name) -> Atom name
  | LitInteger (_line_t, i) -> Int i
  | LitString (_line_t, f) ->
     failwith "unsupported literal LitString"
         
(* [e1; e2; ...] という式の列を let _ = e1 in let _ = e2 ... in という１つの式にする *)
let rec expr_of_exprs = function
  | [] -> unit
  | [e] -> e
  | e :: es ->
     Let ("_", e, expr_of_exprs es)

let rec expr_of_erlang_expr = function
  | F.ExprBody erlangs ->
     expr_of_exprs (List.map expr_of_erlang_expr erlangs)
  | ExprBinOp (_line_t, op, e1, e2) ->
     App(Var op, List.map expr_of_erlang_expr [e1; e2])
  | ExprVar (_line_t, v) -> Var v
  | ExprLit literal -> Val (const_of_literal literal)
     

