open Base
open Common
open Ast_intf
open Result

let new_tyvar () = TyVar (Type_variable.create())

let rec derive context = function
  | Constant c ->
     Ok (TySingleton c, Empty)
  | Var v ->
     begin match Context.find context (Context.Key.Var v) with
     | Some ty ->
        Ok (ty, Empty)
     | None ->
        Error ("unknown type variable: " ^ v)
     end
  | Tuple exprs ->
     result_map_m ~f:(derive context) exprs
     >>| List.unzip
     >>| fun (tys, cs) -> (TyTuple tys, Conj cs)
  | App (f, args) ->
     derive context f >>= fun (tyf, cf) ->
     result_map_m
       ~f:(fun arg ->
           derive context arg >>|
           fun (ty, c) ->
             let alpha = new_tyvar () in
             (alpha, [Subtype (ty, alpha); c]))
       args
     >>= fun derived_form_args_with_alpha ->
     let alpha = new_tyvar () in
     let beta = new_tyvar () in
     let alphas = List.map ~f:fst derived_form_args_with_alpha in
     let args_constraints =
       derived_form_args_with_alpha
       |> List.map ~f:snd
       |> List.concat
     in
     let constraints =
         Eq (tyf, TyFun (alphas, alpha)) ::
         Subtype (beta, alpha) ::
         cf ::
         args_constraints
     in
     Ok (beta, Conj constraints)
  | Abs (vs, e) ->
     let new_tyvars = List.map ~f:(fun v -> (v, (new_tyvar ()))) vs in
     let added_context =
       List.fold_left
         ~f:(fun context (v, tyvar) -> Context.add (Context.Key.Var v) tyvar context)
         ~init:context
         new_tyvars in
     derive added_context e >>= fun (ty_e, c) ->
     let tyvar = new_tyvar () in
     Ok (tyvar, Eq (tyvar, TyConstraint (TyFun (List.map ~f:snd new_tyvars, ty_e), c)))
  | Let (v, e1, e2) ->
     derive context e1 >>= fun (ty_e1, c1) ->
     derive (Context.add (Context.Key.Var v) ty_e1 context) e2 >>= fun (ty_e2, c2) ->
     Ok (ty_e2, Conj [c1; c2])
  | Letrec (lets , e) ->
     let new_tyvars = List.map ~f:(fun (v, f) -> (v, f, (new_tyvar ()))) lets in
     let added_context =
       List.fold_left
         ~f:(fun context (v, _, tyvar) -> Context.add (Context.Key.Var v) tyvar context)
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
  | MFA (Constant (Atom m), Constant (Atom f), Constant (Number a)) ->
     (* find MFA from context *)
     begin match Context.find context (Context.Key.MFA (m, f, a)) with
     | Some ty ->
        Ok (ty, Empty)
     | None ->
        Error (Printf.sprintf "unknown MFA: %s:%s/%d" m f a)
     end
  | MFA (m, f, a) ->
     (* few info to find MFA *)
     let tyvar_mfa = new_tyvar () in
     derive context m >>= fun (ty_m, c_m) ->
     derive context f >>= fun (ty_f, c_f) ->
     derive context a >>= fun (ty_a, c_a) ->
     let cs =
       [c_m; c_f; c_a;
        Subtype (ty_m, TyAtom);
        Subtype (ty_f, TyAtom);
        Subtype (ty_a, TyNumber);
        Subtype (tyvar_mfa, TyAny)] (* cannot be TyFun (.., ..) because the arity is unknown. give up *)
     in
     Ok (tyvar_mfa, Conj cs)
  | other ->
     Error (Printf.sprintf "unsupported type: %s" (show_expr other))
