open Base
open Common
open Ast
open Type
open Result
module C = Constraint

let new_tyvar () = Type.of_elem (TyVar (Type_variable.create()))

(* translate pattern to expression *)
let rec pattern_to_expr = function
  | PatVar (line, v) -> Ref (line, Var v)
  | PatTuple (line, es) -> Tuple (line, es |> List.map ~f:(fun e -> pattern_to_expr e))
  | PatConstant (line, c) -> Constant (line, c)
  | PatCons (line, (p1, p2)) -> ListCons (line, pattern_to_expr p1, pattern_to_expr p2)
  | PatNil line -> ListNil line
  | PatMap (line, assocs) ->
     (* This is temporary implementation. see: https://github.com/dwango/fialyzer/issues/102#issuecomment-461787511 *)
     assocs
     |> List.map ~f:(fun (k, v) -> (pattern_to_expr k, pattern_to_expr v))
     |> (fun kvs -> MapCreation (line, kvs))

(* Extracts variables from given pattern and add new type variable *)
let rec variables_in_pattern = function
  | PatVar (_, v) -> [(v, new_tyvar ())]
  | PatTuple (_, es) -> es |> List.map ~f:(fun e -> variables_in_pattern e)
                      |> List.fold_left ~init:[] ~f:(fun a b -> List.append a b)
  | PatConstant (_, _) -> []
  | PatCons (_, (p1, p2)) -> List.append (variables_in_pattern p1) (variables_in_pattern p2)
  | PatNil _ -> []
  | PatMap (_, assocs) -> assocs |> List.bind ~f:(fun (k, v) -> List.append (variables_in_pattern k) (variables_in_pattern v))

let rec derive context = function
  | Constant (_line, c) ->
     Ok (Type.of_elem (TySingleton c), C.Empty)
  | Ref (line, Var v) ->
     begin match Context.find context (Context.Key.Var v) with
     | Some ty ->
        Ok (ty, C.Empty)
     | None ->
        let filename = "TODO:filename" in
        let line_in_file = line in
        Error Known_error.(FialyzerError (UnboundVariable {filename; line=line_in_file; variable=Var v}))
     end
  | Tuple (_line, exprs) ->
     result_map_m ~f:(derive context) exprs
     >>| List.unzip
     >>| fun (tys, cs) -> (Type.of_elem (TyTuple tys), C.Conj cs)
  | App (_line, f, args) as app ->
     derive context f >>= fun (tyf, cf) ->
     result_map_m
       ~f:(fun arg ->
           derive context arg >>|
           fun (ty, c) ->
             let alpha = new_tyvar () in
             (alpha, [C.Subtype {lhs=ty; rhs=alpha; link=arg}; c]))
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
         C.Eq {lhs=tyf; rhs=Type.of_elem (TyFun (alphas, alpha)); link=f} ::
         C.Subtype {lhs=beta; rhs=alpha; link=app} ::
         cf ::
         args_constraints
     in
     Ok (beta, C.Conj constraints)
  | Abs (_line, {args=vs; body=e}) ->
     let new_tyvars = List.map ~f:(fun v -> (v, (new_tyvar ()))) vs in
     let added_context =
       List.fold_left
         ~f:(fun context (v, tyvar) -> Context.add (Context.Key.Var v) tyvar context)
         ~init:context
         new_tyvars
     in
     derive added_context e >>= fun (ty_e, c) ->
     Ok (Type.of_elem (TyFun (List.map ~f:snd new_tyvars, ty_e)), c)
  | Let (_line, v, e1, e2) ->
     derive context e1 >>= fun (ty_e1, c1) ->
     derive (Context.add (Context.Key.Var v) ty_e1 context) e2 >>= fun (ty_e2, c2) ->
     Ok (ty_e2, C.Conj [c1; c2])
  | Letrec (line, lets , e) ->
     let new_tyvars = List.map ~f:(fun (v, f) -> (v, f, (new_tyvar ()))) lets in
     let added_context =
       List.fold_left
         ~f:(fun ctx (ref_, {args; _}, tyvar) ->
           let key =
             begin match ref_ with
             | Var v ->
                Context.Key.Var v
             | LocalFun {function_name; arity} ->
                Context.Key.LocalFun {function_name; arity}
             | MFA _ ->
                failwith "cannot reach here"
             end
           in
           Context.add key tyvar ctx)
         ~init:context
         new_tyvars
     in
     let constraints_result =
       new_tyvars
       |> result_map_m ~f:(fun (_, f, tyvar) -> derive added_context (Abs (line, f)) >>| fun(ty, c) -> (Abs (line, f), ty, c, tyvar))
       >>| List.map ~f:(fun (abs, ty, c, tyvar) -> [C.Eq {lhs=tyvar; rhs=ty; link=abs}; c])
       >>| List.concat
     in
     constraints_result >>= fun constraints ->
     derive added_context e >>= fun (ty, c) ->
     Ok (ty, C.Conj (c :: constraints))
  | Ref (_, LocalFun {function_name; arity}) ->
     let key = Context.Key.LocalFun {function_name; arity} in
     begin match Context.find context key with
     | Some ty -> Ok (ty, C.Empty)
     | None ->
        let filename = "TODO:filename" in
        let line = -1 (*TODO: line*) in
        Error Known_error.(FialyzerError (UnboundVariable {filename; line; variable=key}))
     end
  | Ref (_, MFA {module_name = Constant (_line_m, Atom m); function_name = Constant (_line_f, Atom f); arity = Constant (line_a, Number (Int a))}) ->
     (* find MFA from context *)
     let mfa = Context.Key.MFA {module_name=m; function_name=f; arity=a} in
     begin match Context.find context mfa with
     | Some ty ->
        Ok (ty, C.Empty)
     | None ->
        let filename = "TODO:filename" in
        let line = -1 (*TODO: line*) in
        Error Known_error.(FialyzerError (UnboundVariable {filename; line; variable=mfa}))
     end
  | Ref (_, MFA {module_name=m; function_name=f; arity=a}) ->
     (* few info to find MFA *)
     let tyvar_mfa = new_tyvar () in
     derive context m >>= fun (ty_m, c_m) ->
     derive context f >>= fun (ty_f, c_f) ->
     derive context a >>= fun (ty_a, c_a) ->
     let cs =
       [c_m; c_f; c_a;
        C.Subtype {lhs=ty_m; rhs=Type.of_elem TyAtom; link=m};
        C.Subtype {lhs=ty_f; rhs=Type.of_elem TyAtom; link=f};
        C.Subtype {lhs=ty_a; rhs=Type.of_elem TyNumber; link=a};
       ]
     in
     Ok (tyvar_mfa, C.Conj cs)
  | Case (line, e, clauses) ->
    derive context e >>= fun (ty_e_t, c_e) ->
    let beta = new_tyvar () in
    let results = clauses |> result_map_m ~f:(fun ((p_n, g_n), b_n) ->
      let p_n_tyvars = variables_in_pattern p_n in
      (* A ∪ { ... } *)
      let added_context =
        List.fold_left
        ~f:(fun context (v, tyvar) -> Context.add (Context.Key.Var v) tyvar context)
        ~init:context
        p_n_tyvars in
      (* |- p_n : alpha_n, c_p *)
      derive_pattern added_context p_n >>= fun(ty_alpha_n, c_p) ->
      (* PAT *)
      derive added_context g_n >>= fun(ty_g_n, c_g) ->
      (* |- b_n : beta_n *)
      derive added_context b_n >>= fun(ty_beta_n, c_b) ->
        Ok (C.Conj [
          (*
            In the original paper about success typing, it's assumed that guard are true. However, we consider
            guards to be boolean type because true type constraint is too strong and not intuitive.
            Boolean type is true | false.
           *)
          C.Subtype {lhs=ty_g_n; rhs=Type.bool; link=g_n};
          C.Eq {lhs=beta; rhs=ty_beta_n; link=b_n};
          C.Eq {lhs=ty_e_t; rhs=ty_alpha_n; link=e}; c_p; c_g; c_b
        ])
    ) in
    results >>= fun(cs) -> Ok (beta, C.Conj [C.Disj cs; c_e])
  | ListCons (line, hd, tl) ->
    let alpha = new_tyvar() in
    derive context hd >>= fun (ty_hd, c_hd) ->
    derive context tl >>= fun (ty_tl, c_tl) ->
    Ok (Type.of_elem (TyList (Type.sup alpha ty_hd)), C.Conj [
      C.Eq {lhs=ty_tl; rhs=Type.of_elem (TyList alpha); link=tl};
      c_hd;
      c_tl
    ])
  | ListNil line ->
    Ok (Type.of_elem (TyList TyBottom), C.Empty)
  | MapCreation (_line, _assocs) ->
     (* TODO: fully support map creation. see: https://github.com/dwango/fialyzer/issues/102#issuecomment-461787511 *)
     Ok (Type.of_elem TyAnyMap, Empty)
  | MapUpdate {map; _} ->
     (* TODO: fully support map update. see: https://github.com/dwango/fialyzer/issues/102#issuecomment-461787511 *)
     derive context map >>= fun (ty_map, c_map) ->
     Ok (Type.of_elem TyAnyMap, C.Conj [c_map; C.Subtype {lhs=ty_map; rhs=Type.of_elem TyAnyMap; link=map}])
  | Catch (line, body) ->
     derive context body >>= fun (_, c) ->
     Ok (Type.TyAny, c)
and derive_pattern context pattern = 
    begin match pattern with
    | PatVar (_, _) | PatTuple (_, _) | PatConstant (_, _) | PatCons (_, (_, _)) | PatNil _ ->
      derive context (pattern_to_expr pattern)
    | PatMap _ ->
      Ok (Type.of_elem TyAnyMap, Empty)
    end
