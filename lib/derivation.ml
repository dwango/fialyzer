open Base
open Common
open Ast_intf
open Result

let new_tyvar () = TyVar (Type_variable.create())

let rec derive context = function
  | Val c ->
     Ok (TyConstant c, Empty)
  | Var v ->
     begin match Context.find context v with
     | Some ty ->
        Ok (ty, Empty)
     | None ->
        Error ("unknown type variable: " ^ v)
     end
  (* TODO: implement derivation of application expression with multiple arguments *)
  | App (f, [arg1]) ->
     derive context f >>= fun (tyf, cf) ->
     derive context arg1 >>= fun (ty1, c1) ->
     let alpha1 = new_tyvar () in
     let alpha = new_tyvar () in
     let beta = new_tyvar () in
     let constraints = [
         Eq (tyf, TyFun ([alpha1], alpha));
         Subtype (beta, alpha);
         Subtype (ty1, alpha1);
         cf;
         c1;
       ]
     in
     Ok (beta, Conj constraints)
  | Abs (vs, e) ->
     let new_tyvars = List.map ~f:(fun v -> (v, (new_tyvar ()))) vs in
     let added_context =
       List.fold_left
         ~f:(fun context (v, tyvar) -> Context.add v tyvar context)
         ~init:context
         new_tyvars in
     derive added_context e >>= fun (ty_e, c) ->
     let tyvar = new_tyvar () in
     Ok (tyvar, Eq (tyvar, TyConstraint (TyFun (List.map ~f:snd new_tyvars, ty_e), c)))
  | Let (v, e1, e2) ->
     derive context e1 >>= fun (ty_e1, c1) ->
     derive (Context.add v ty_e1 context) e2 >>= fun (ty_e2, c2) ->
     Ok (ty_e2, Conj [c1; c2])
  | Letrec (lets , e) ->
     let new_tyvars = List.map ~f:(fun (v, f) -> (v, f, (new_tyvar ()))) lets in
     let added_context =
       List.fold_left
         ~f:(fun context (v, _, tyvar) -> Context.add v tyvar context)
         ~init:context
         new_tyvars in
     let constraints_result =
       new_tyvars
       |> result_map_m ~f:(fun (_, f, tyvar) -> derive added_context f >>| fun(ty, c) -> (ty, c, tyvar))
       >>| List.map ~f:(fun (ty, c, tyvar) -> [Eq (tyvar, ty); c])
       >>| List.concat
     in
     constraints_result >>= fun constraints ->
     derive added_context e >>= fun (ty, c) ->
     Ok (ty, Conj (c :: constraints))
  | other ->
     Error (Printf.sprintf "unsupported type: %s" (show_expr other))
