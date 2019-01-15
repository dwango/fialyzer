open Base
open Ast
open Obeam
open Polymorphic_compare
module F = Abstract_format
open Common

let unit : Ast.t = Constant (Number 0)

let const_of_literal = function
  | F.LitAtom (_line_t, name) -> Constant.Atom name
  | LitInteger (_line_t, i) -> Constant.Number i
  | LitBigInt (_line_t, z) ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/93"]; message="support bigint literal"}))
  | LitString (_line_t, _s) ->
     (* treat in expr_of_literal, etc *)
     failwith "cannot reach here"

let expr_of_literal = function
  | F.LitString (_line_t, s) ->
     (* string is a list of chars in Erlang *)
     let l = String.to_list_rev s in
     List.fold_left l ~init:ListNil ~f:(fun acc c -> ListCons (Constant (Number (Char.to_int c)), acc))
  | l -> Constant (const_of_literal l)

let pattern_of_literal = function
  | F.LitString (_line_t, s) ->
     (* string is a list of chars in Erlang *)
     let l = String.to_list_rev s in
     List.fold_left l ~init:PatNil ~f:(fun acc c -> PatCons (PatConstant (Number (Char.to_int c)), acc))
  | l -> PatConstant (const_of_literal l)

(* Extracts nested match expressions.
 * e.g.,
 * extract_match_expr (A = f(B = C)) ===> B = C, A = f(C)
 *)
let rec extract_toplevel e = match extract_match_expr e with
  | [F.ExprBody es] -> F.ExprBody es
  | es -> F.ExprBody es
and extract_match_expr e =
  let extract_clause = function
    | F.ClsCase (line, p, g, e) ->
       let e' = extract_toplevel e in
       F.ClsCase (line, p, g, e')
    | ClsFun (line, ps, g, e) ->
       let e' = extract_toplevel e in
       F.ClsFun (line, ps, g, e')
  in
  let return_expr is_top acc e =
    if is_top then (e :: acc, e) else (acc, e)
  in
  let rec extract_match_expr' acc is_top = function
    | F.ExprBody es ->
       F.ExprBody List.(es >>= extract_match_expr)
       |> return_expr is_top acc
    | ExprCase (line, e, cs) ->
       let (acc, e') = extract_match_expr' acc false e in
       let cs' = List.map ~f:extract_clause cs in
       F.ExprCase (line, e', cs')
       |> return_expr is_top acc
    | ExprCons (line, e1, e2) ->
       let (acc, e1') = extract_match_expr' acc false e1 in
       let (acc, e2') = extract_match_expr' acc false e2 in
       F.ExprCons (line, e1', e2')
       |> return_expr is_top acc
    | ExprNil _ as e -> return_expr is_top acc e
    | ExprListComprehension (line, e, quals) ->
       (* TODO: support list comprehension *)
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/92"];
                                                         message="support list comprehension `[E_0 || Q_1, ..., Q_k]`"}))
    | ExprLocalFunRef _ as e -> return_expr is_top acc e
    | ExprRemoteFunRef _ as e -> return_expr is_top acc e
    | ExprFun (line, name, cs) ->
       let cs' = List.map ~f:extract_clause cs in
       F.ExprFun (line, name, cs')
       |> return_expr is_top acc
    | ExprLocalCall (line, f, args) ->
       let (acc, f') = extract_match_expr' acc false f in
       (* NOTE: Evaluation of match expression arguments does not affect each other.
        * e.g., `f(A = 1, B = A)` cannot be compiled, and `f(A = B, B = 1)` also.
        * So we convert `f(A = 1, B = 1)` to `B = 1, A = 1, f(1, 1)` because it is
        * guaranteed by Erlang compiler that new variables introduced in a local-call
        * argument are not used in another argument.
        *)
       let (acc, args') =
         List.fold_right args ~init:(acc, []) ~f:(fun arg (acc, args) ->
                           let (acc, arg') = extract_match_expr' acc false arg in
                           (acc, arg' :: args))
       in
       F.ExprLocalCall (line, f', args')
       |> return_expr is_top acc
    | ExprRemoteCall (line, line_m, m, f, args) ->
       let (acc, m') = extract_match_expr' acc false m in
       let (acc, f') = extract_match_expr' acc false f in
       (* same as ExprLocalCall comment *)
       let (acc, args') =
         List.fold_right args ~init:(acc, []) ~f:(fun arg (acc, args) ->
                           let (acc, arg') = extract_match_expr' acc false arg in
                           (acc, arg' :: args))
       in
       F.ExprRemoteCall (line, line_m, m', f', args')
       |> return_expr is_top acc
    | ExprMapCreation (line, assocs) ->
       (* NOTE: Evaluation of match expressions in assocs does not affect each other.
        * e.g., `#{K1 = K2 => V1 = 1, K2 = 1 => V2 = V1}` cannot be compiled.
        * So we convert `#{K1 = k1 => V1 = v1, K2 = k2 => V2 = v2}` to
        * `K2 = k2, V2 = v2, K1 = k1, V1 = v1, #{k1 => v1, k2 => v2}` because it is
        * guaranteed by Erlang compiler that new variables introduced in an assoc
        * are not used in another assoc.
        *)
       let (acc, assocs') =
         List.fold_right assocs ~init:(acc, []) ~f:(fun assoc (acc, assocs) ->
                           let (acc, assoc') = extract_assoc acc assoc in
                           (acc, assoc' :: assocs))
       in
       F.ExprMapCreation (line, assocs')
       |> return_expr is_top acc
    | ExprMapUpdate (line, m, assocs) ->
       let (acc, m') = extract_match_expr' acc false m in
       (* same as ExprMapCreation comment *)
       let (acc, assocs') =
         List.fold_right assocs ~init:(acc, []) ~f:(fun assoc (acc, assocs) ->
                           let (acc, assoc') = extract_assoc acc assoc in
                           (acc, assoc' :: assocs))
       in
       F.ExprMapUpdate (line, m', assocs')
       |> return_expr is_top acc
    | ExprMatch (line, p, e) ->
       let (acc, e') = extract_match_expr' acc false e in
       (* Make ExprMatch appeared to top in even if is_top=false *)
       (ExprMatch (line, p, e') :: acc, e')
    | ExprBinOp (line, op, e1, e2) ->
       let (acc, e1') = extract_match_expr' acc false e1 in
       let (acc, e2') = extract_match_expr' acc false e2 in
       F.ExprBinOp (line, op, e1', e2')
       |> return_expr is_top acc
    | ExprTuple (line, es) ->
       (* NOTE: Evaluation of match expression in tuple elements does not affect each other.
        * e.g., `{A = 1, B = A}` cannot be compiled, and `{A = B, B = 1}` also.
        * So we convert `{A = 1, B = 1}` to `B = 1, A = 1, {1, 1}` because it is
        * guaranteed by Erlang compiler that new variables introduced in a tuple
        * element are not used in another element.
        *)
       let (acc, es') =
         List.fold_right es ~init:(acc, []) ~f:(fun e (acc, es) ->
                           let (acc, e') = extract_match_expr' acc false e in
                           (acc, e' :: es))
       in
       F.ExprTuple (line, es')
       |> return_expr is_top acc
    | ExprVar _ as e -> return_expr is_top acc e
    | ExprLit _ as e -> return_expr is_top acc e
  and extract_assoc acc = function
    (* is_top=false because assocs are not appeared to top-level *)
    | F.ExprAssoc (line, k, v) ->
       let (acc, k') = extract_match_expr' acc false k in
       let (acc, v') = extract_match_expr' acc false v in
       (acc, F.ExprAssoc (line, k', v'))
    | F.ExprAssocExact (line, k, v) ->
       let (acc, k') = extract_match_expr' acc false k in
       let (acc, v') = extract_match_expr' acc false v in
       (acc, F.ExprAssocExact (line, k', v'))
  in
  let (es, _) = extract_match_expr' [] true e in
  List.rev es

let expr_of_atom_or_var = function
  | F.AtomVarAtom (_line_t, a) -> Constant (Atom a)
  | AtomVarVar (_line_t, v) -> Var v

let expr_of_integer_or_var = function
  | F.IntegerVarInteger (_line_t, i) -> Constant (Number i)
  | IntegerVarVar (_line_t, v) -> Var v

let rec pattern_of_erlang_pattern = function
  | F.PatVar (_, v) -> Ast.PatVar v
  | F.PatUniversal _ -> Ast.PatVar "_"
  | F.PatLit literal -> pattern_of_literal literal
  | F.PatMap _ ->
     let issue_links = ["https://github.com/dwango/fialyzer/issues/102"] in
     let message = "support map pattern" in
     raise Known_error.(FialyzerError (NotImplemented {issue_links; message}))
  | F.PatTuple (_, patterns) ->
     PatTuple (patterns |> List.map ~f:pattern_of_erlang_pattern)
  | F.PatNil _ -> PatNil
  | F.PatCons (_, p1, p2) ->
     PatCons (pattern_of_erlang_pattern p1, pattern_of_erlang_pattern p2)


(* converts a secuence of expressions `[e1; e2; ...]` to an expression `let _ = e1 in let _ = e2 in ...` *)
(* assume `extract_toplevel` is applied to the argument *)
let rec expr_of_erlang_exprs = function
  | [] -> unit
  | [e] -> expr_of_erlang_expr' e
  | F.ExprMatch (_line, p, e) :: es ->
     (* no match expression in `e` by extract_match_expr *)
     let e' = expr_of_erlang_expr' e in
     let es' = expr_of_erlang_exprs es in
     Case (e', [((pattern_of_erlang_pattern p, Constant (Atom "true")), es')])
  | e :: es ->
     Let ("_", expr_of_erlang_expr' e, expr_of_erlang_exprs es)
and expr_of_erlang_expr' = function
  | F.ExprBody erlangs ->
     expr_of_erlang_exprs erlangs
  | ExprCase (line, e, clauses) ->
     let cs = clauses |> List.map ~f:(function
       | F.ClsCase (_, pattern, guard, e) ->
          if Option.is_some guard then Log.debug [%here] "line:%d %s" line "Guard (when clauses) are not supported";
          ((pattern_of_erlang_pattern pattern, Constant (Atom "true")), expr_of_erlang_expr' e)
       | F.ClsFun (_, _, _, _) ->
          failwith "cannot reach here"
    ) in
    Case (expr_of_erlang_expr' e, cs)
  | ExprLocalFunRef (_line_t, name, arity) ->
     (* TODO: support local `fun F/A` *)
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/79"]; message="support local `fun f/A`"}))
  | ExprRemoteFunRef (_line_t, m, f, a) ->
    MFA {module_name = expr_of_atom_or_var m;  function_name = expr_of_atom_or_var f; arity = expr_of_integer_or_var a}
  | ExprFun (_line_t, name_option, clauses) ->
    (* Create a list which have n elements *)
    let rec fill e = (function
    | 0 -> []
    | n -> (e())::(fill e (n - 1))
    )
    in
    let (cs, arities) = clauses |> List.map ~f:(function
    | F.ClsCase (_, _, _, _) -> failwith "cannot reach here"
    | F.ClsFun (_, patterns, _, body) ->
      (* Ignore guards currently since guard is complex and it's not needed for simple examples *)
      let ps = patterns |> List.map ~f:pattern_of_erlang_pattern in
      let arity = List.length ps in
      let tuple_pattern = PatTuple ps in
      (((tuple_pattern, Constant (Atom ("true"))), expr_of_erlang_expr' body), arity)
    ) |> List.unzip in
     let make_fresh_variables length = fill (fun () -> Variable.create()) length |> List.rev in
     let make_case cs fresh_variables =
       let fresh_tuple = Tuple (fresh_variables |> List.map ~f:(fun v -> Var v)) in
       (* letrec $name = fun $name(A1, A2, ...) -> b1; $name(B1, B2, ...)-> b2; ... end in $name *)
       Case (fresh_tuple, cs)
     in
     let function_body = match cs with
     | ((PatTuple patterns, Constant (Atom ("true"))), body)::[] ->
       let all_pattern_is_var = patterns |> List.for_all ~f:(function
       | PatVar _ -> true
       | _ -> false
       ) in
       if all_pattern_is_var then
         let args = patterns |> List.map ~f:(function
         | PatVar v -> v
         | _ -> failwith "cannot reach here"
         ) in
         Abs (args, body)
       else
         let arity = List.length patterns in
         let fresh_variables: string list = (make_fresh_variables arity) in
         Abs (fresh_variables, make_case cs fresh_variables)
     | cs ->
       (* Assume all arities have the same value *)
       let arity = List.nth_exn arities 0 in
       let fresh_variables = (make_fresh_variables arity) in
       Abs (fresh_variables, make_case cs fresh_variables)
     in
     (* If name is omitted, don't create Letrec *)
     (match name_option with
     | Some name -> Letrec ([(name, function_body)], Var name)
     | None -> function_body
     )
  | ExprLocalCall (_line_t, ExprLit (LitAtom (_line_f, fun_name)), args) ->
     App (Var fun_name, List.map ~f:expr_of_erlang_expr' args)
  | ExprLocalCall (_line_t, f, args) ->
     App (expr_of_erlang_expr' f, List.map ~f:expr_of_erlang_expr' args)
  | ExprRemoteCall (_line_t, _line_m, m, f, args) ->
     let mfa = MFA {
       module_name=expr_of_erlang_expr' m;
       function_name=expr_of_erlang_expr' f;
       arity=Constant (Number (List.length args))} in
     App (mfa, List.map ~f:expr_of_erlang_expr' args)
  | ExprMatch (line_t, pat, e) ->
     (* There is no match expression in `e` by `extract_match_expr`.
      * Futhermore, this match expression is single or the last of expr sequence because
      * a match expression which has subsequent expressions is caught in `expr_of_erlang_exprs`.
      * Therefore, we can put right-hand side expr of match expression to the return value of case expr.
      *)
     let e' = expr_of_erlang_expr' e in
     Case (e', [((pattern_of_erlang_pattern pat, Constant (Atom "true")), e')])
  | ExprBinOp (_line_t, op, e1, e2) ->
     let func = Ast.MFA {
        module_name = Constant (Atom "erlang");
        function_name = Constant (Atom op);
        arity=Constant (Number 2)
     } in
     App(func, List.map ~f:expr_of_erlang_expr' [e1; e2])
  | ExprTuple (_line_t, es) ->
     Tuple (List.map ~f:expr_of_erlang_expr' es)
  | ExprVar (_line_t, v) -> Var v
  | ExprLit literal -> expr_of_literal literal
  | ExprCons (_line_t, e1, e2) -> ListCons (expr_of_erlang_expr' e1, expr_of_erlang_expr' e2)
  | ExprNil _line_t -> ListNil
  | ExprListComprehension (_line_t, expr, quals) ->
     (* TODO: support list comprehension *)
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/92"]; message="support list comprehension `[E_0 || Q_1, ..., Q_k]`"}))
  | ExprMapCreation (_, _) | ExprMapUpdate (_, _, _) ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/122"]; message="support map-related expression"}))

let expr_of_erlang_expr e =
  e |> extract_toplevel |> expr_of_erlang_expr'

let clauses_to_function = function
  | F.ClsCase(_line, _pattern, _guards, _body) ->
     failwith "not implemented: Clause Case"(* TODO : clause case *)
  | F.ClsFun(_line, args, _guards, body) ->
     let f = (function
     | F.PatVar (_, v) -> v
     | F.PatUniversal _ -> "_"
     | F.PatMap (_, _) | F.PatLit _ | F.PatTuple (_, _) | F.PatCons (_, _, _) | F.PatNil _ ->
        let link = "https://github.com/dwango/fialyzer/issues/121" in
        let msg = "support map, literal, tuple, and list patterns in top-level function" in
        raise Known_error.(FialyzerError (NotImplemented {issue_links=[link]; message=msg}))
     ) in
     let vs = args |> List.map ~f:f in
     (vs, expr_of_erlang_expr body)

let forms_to_functions forms =
  let find_specs fun_name =
    List.find_map ~f:(function
                      | F.SpecFun (_line, _mod_name, fname, arity, specs) when fun_name = fname ->
                         List.map ~f:(fun ty ->
                                    match Type.of_erlang ty with
                                    | Type.(TyUnion [Type.TyFun (domains, range)]) -> (domains, range)
                                    | _ -> failwith (!%"unexpected type spec of %s" fname))
                                  specs
                         |> Option.return
                      | _ -> None) forms
  in
  forms
  |> List.filter_map ~f:(function F.DeclFun(line, name, arity, clauses) -> Some(line, name, arity, clauses) | _ -> None)
  |> List.map ~f:(fun (_line, name, arity, clauses) ->
                let specs = find_specs name in
                let (vs, body) = clauses_to_function (List.hd_exn clauses) (* TODO : multi clauses function *) in
                {specs; fun_name=name; args=vs; body})

let forms_to_module forms =
  let take_file forms =
    List.find_map ~f:(function F.AttrFile(line, file, line2) -> Some(line, file, line2) | _ -> None) forms
    |> Result.of_option ~error:(Failure "file attribute not found")
  in
  let take_module_name forms =
    List.find_map ~f:(function F.AttrMod(line, name) -> Some(line, name) | _ -> None) forms
    |> Result.of_option ~error:(Failure "module attribute not found")
  in
  let open Result in
  take_file forms >>= fun (_line, file, line2) ->
  take_module_name forms >>= fun (_line, name) ->
  let functions = forms_to_functions forms in
  let export = [] in (* TODO : take export functions *)
  Result.return {file; name; export; functions }

let code_to_module (F.AbstractCode form) =
  match form with
  | F.ModDecl forms ->
     forms_to_module forms
  | _ ->
     failwith "except for module decl, it is out of support"

let module_to_expr m =
  let funs =
    m.functions |> List.map ~f:(fun {specs; fun_name=name; args; body} ->
                              (name, Abs (args, body)))
  in
  let fun_names = funs |> List.map ~f:fst in
  Letrec(funs, Tuple (List.map ~f:(fun name -> Var name) fun_names))
  |> Result.return

let code_to_expr code =
  let open Result in
  code_to_module code >>= fun m ->
  module_to_expr m
