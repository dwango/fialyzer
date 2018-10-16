open Base
open Types

let new_var =
  let i = ref 0 in
  fun () ->
    Int.incr i;
    Printf.sprintf "%s%02d" "v" !i

let rec derive context = function
  | Types.Val c ->
     Ok (Types.TyConstant c, Types.Empty)
  | Types.Var v ->
     begin match Context.find context v with
     | Some ty ->
        Ok(ty, Types.Empty)
     | None ->
        Error ("unknown type variable: " ^ v)
     end
  | Types.App (f, [arg1]) ->
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
  | Types.Abs ([v], e) ->
     let open Result in
     let ty_v = new_var () in
     derive (Context.add v (Types.TyVar ty_v) context) e >>= fun (ty_e, c) ->
     Ok(Types.TyFun([Types.TyVar ty_v], Types.TyConstraint(ty_e, c)), Types.Empty)
  | other ->
     Error (Printf.sprintf "unsupported type: %s" (Types.show_expr other))
