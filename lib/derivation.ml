open Base
open Common
open Ast
open Type
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
        let filename = "TODO:filename" in
        let line = -1 (*TODO: line*) in
        Error Known_error.(FialyzerError (UnboundVariable {filename; line; variable=Var v}))
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
     Ok (TyFun (List.map ~f:snd new_tyvars, ty_e), c)
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
  | MFA {module_name = Constant (Atom m); function_name = Constant (Atom f); arity = Constant (Number a)} ->
     (* find MFA from context *)
     let mfa = Context.Key.MFA {module_name=m; function_name=f; arity=a} in
     begin match Context.find context mfa with
     | Some ty ->
        Ok (ty, Empty)
     | None ->
        let filename = "TODO:filename" in
        let line = -1 (*TODO: line*) in
        Error Known_error.(FialyzerError (UnboundVariable {filename; line; variable=mfa}))
     end
  | MFA {module_name=m; function_name=f; arity=a} ->
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
  | Case (e, clauses) ->
    (* translate pattern to expression *)
    let rec pattern_to_expr = function
      | PatVar v -> Var v
      | PatTuple es -> Tuple (es |> List.map ~f:(fun e -> pattern_to_expr e))
      | PatConstant c -> Constant c
      | PatCons (p1, p2) -> ListCons (pattern_to_expr p1, pattern_to_expr p2)
      | PatNil -> ListNil
    in
    (* Var(p) *)
    let rec variables = function
      | PatVar v -> [(v, new_tyvar ())]
      | PatTuple es -> es |> List.map ~f:(fun e -> variables e)
                          |> List.fold_left ~init:[] ~f:(fun a b -> List.append a b)
      | PatConstant _ -> []
      | PatCons (p1, p2) -> List.append (variables p1) (variables p2)
      | PatNil -> []
    in
    derive context e >>= fun (ty_e_t, c_e) ->
    let beta = new_tyvar () in
    let results = clauses |> result_map_m ~f:(fun ((p_n, g_n), b_n) ->
      let p_n_tyvars = variables p_n in
      let p_n_expr = pattern_to_expr p_n in
      (* A ∪ { ... } *)
      let added_context =
        List.fold_left
        ~f:(fun context (v, tyvar) -> Context.add (Context.Key.Var v) tyvar context)
        ~init:context
        p_n_tyvars in
      (* |- p_n : alpha_n, c_p *)
      derive added_context p_n_expr >>= fun(ty_alpha_n, c_p) ->
      (* PAT *)
      derive added_context g_n >>= fun(ty_g_n, c_g) ->
      (* |- b_n : beta_n *)
      derive added_context b_n >>= fun(ty_beta_n, c_b) ->
        Ok (Conj [
          (*
            In the original paper about success typing, it's assumed that guard are true. However, we consider
            guards to be boolean type because true type constraint is too strong and not intuitive.
            Boolean type is true | false.
           *)
          Subtype (ty_g_n, TyUnion (TySingleton (Atom "true"), TySingleton (Atom "false")));
          Eq (beta, ty_beta_n); Eq (ty_e_t, ty_alpha_n); c_p; c_g; c_b
        ])
    ) in
    results >>= fun(cs) -> Ok (beta, Conj [Disj cs; c_e])
  | ListCons (_e1, _e2) ->
     (* TODO: support cons expr *)
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/90"]; message="support cons expr"}))
  | ListNil ->
     (* TODO: support nil expr *)
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/90"]; message="support nil expr"}))
