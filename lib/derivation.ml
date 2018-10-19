open Base
open Ast_intf

let new_var =
  let i = ref 0 in
  fun () ->
    Int.incr i;
    Printf.sprintf "%s%02d" "v" !i

let rec derive context = function
  | Val c ->
     Ok (TyConstant c, Empty)
  | Var v ->
     begin match Context.find context v with
     | Some ty ->
        Ok(ty, Empty)
     | None ->
        Error ("unknown type variable: " ^ v)
     end
  | App (f, [arg1]) ->
     let open Result in
     derive context f >>= fun (tyf, cf) ->
     derive context arg1 >>= fun (ty1, c1) ->
     let alpha1 = new_var() in
     let alpha = new_var() in
     let beta = new_var() in
     let constraints = [
         Eq (tyf, TyFun ([TyVar alpha1], TyVar alpha));
         Subtype (TyVar beta, TyVar alpha);
         Subtype (ty1, TyVar alpha1);
         cf;
         c1;
       ]
     in
     Ok(TyVar beta, Conj constraints)
  | Abs ([v], e) ->
     let open Result in
     let ty_v = new_var () in
     derive (Context.add v (TyVar ty_v) context) e >>= fun (ty_e, c) ->
     Ok(TyFun([TyVar ty_v], TyConstraint(ty_e, c)), Empty)
  | other ->
     Error (Printf.sprintf "unsupported type: %s" (show_expr other))
