open Base
open Ast
open Obeam
open Poly
module F = Abstract_format
open Common

let unit : Ast.t = Constant (-1, Number (Int 0))

let const_of_literal = function
  | F.LitAtom {line; atom} -> (line, Constant.Atom atom)
  | LitChar {line; uchar} ->
     (line, Constant.Number (Int (Uchar.to_scalar uchar)))
  | LitFloat {line; float} ->
     (line, Constant.Number (Float float))
  | LitInteger {line; integer} -> (line, Constant.Number (Int integer))
  | LitBigInt _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/93"]; message="support bigint literal"}))
  | LitString _ ->
     (* treat in expr_of_literal, etc *)
     failwith "cannot reach here"

let rev_list_of_chars = function
  | F.Asciis s -> String.to_list_rev s |> List.map ~f:Char.to_int
  | F.CharList cs -> List.rev cs

let expr_of_literal = function
  | F.LitString {line; str} ->
     (* string is a list of chars in Erlang *)
     rev_list_of_chars str
     |> List.map ~f:(fun i -> Constant (line, Number (Int i)))
     |> List.fold_left ~init:(ListNil line) ~f:(fun tl hd -> ListCons (line, hd, tl))
  | l ->
     let (line, c) = const_of_literal l in
     Constant(line, c)

let pattern_of_literal = function
  | F.LitString {line; str} ->
     (* string is a list of chars in Erlang *)
     rev_list_of_chars str
     |> List.map ~f:(fun i -> PatConstant (line, (Number (Int i))))
     |> List.fold_left ~init:(PatNil line) ~f:(fun tl hd -> PatCons (line, (hd, tl)))
  | l ->
     let (line, c) = const_of_literal l in
     PatConstant (line, c)

(* Extracts nested match expressions.
 * e.g.,
 * extract_match_expr (A = f(B = C)) ===> B = C, A = f(C)
 *)
let rec extract_toplevel e = match extract_match_expr e with
  | [F.ExprBody {exprs}] -> F.ExprBody {exprs}
  | exprs -> F.ExprBody {exprs}
and extract_match_expr e =
  let extract_clause = function
    | F.ClsCase c ->
       let body' = extract_toplevel c.body in
       F.ClsCase {c with body = body'}
    | ClsFun c ->
       let body' = extract_toplevel c.body in
       F.ClsFun {c with body = body'}
    | ClsCatch _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/223"];
                                                         message="support try expr"}))
    | ClsIf _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/224"];
                                                         message="support if expr"}))
  in
  let return_expr is_top acc e =
    if is_top then (e :: acc, e) else (acc, e)
  in
  let rec extract_match_expr' acc is_top = function
    | F.ExprBody {exprs} ->
       F.ExprBody {exprs = List.(exprs >>= extract_match_expr)}
       |> return_expr is_top acc
    | ExprBitstr _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/220"];
                                                         message="support bitstring"}))
    | ExprBitstrComprehension _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/221"];
                                                         message="support bitstring comprehension"}))
    | ExprBlock _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/222"];
                                                         message="support block expr"}))
    | ExprCase e ->
       let (acc, expr') = extract_match_expr' acc false e.expr in
       let cs' = List.map ~f:extract_clause e.clauses in
       F.ExprCase {e with expr = expr'; clauses = cs'}
       |> return_expr is_top acc
    | ExprCatch _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/223"];
                                                         message="support catch"}))
    | ExprCons e ->
       let (acc, h') = extract_match_expr' acc false e.head in
       let (acc, t') = extract_match_expr' acc false e.tail in
       F.ExprCons {e with head = h'; tail = t'}
       |> return_expr is_top acc
    | ExprNil _ as e -> return_expr is_top acc e
    | ExprListComprehension _ ->
       (* TODO: support list comprehension *)
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/92"];
                                                         message="support list comprehension `[E_0 || Q_1, ..., Q_k]`"}))
    | ExprLocalFunRef _ as e -> return_expr is_top acc e
    | ExprRemoteFunRef _ as e -> return_expr is_top acc e
    | ExprFun e ->
       let cs' = List.map ~f:extract_clause e.clauses in
       F.ExprFun {e with clauses = cs'}
       |> return_expr is_top acc
    | ExprLocalCall e ->
       let (acc, f') = extract_match_expr' acc false e.function_expr in
       (* NOTE: Evaluation of match expression arguments does not affect each other.
        * e.g., `f(A = 1, B = A)` cannot be compiled, and `f(A = B, B = 1)` also.
        * So we convert `f(A = 1, B = 1)` to `B = 1, A = 1, f(1, 1)` because it is
        * guaranteed by Erlang compiler that new variables introduced in a local-call
        * argument are not used in another argument.
        *)
       let (acc, args') =
         List.fold_right e.args ~init:(acc, []) ~f:(fun arg (acc, args) ->
                           let (acc, arg') = extract_match_expr' acc false arg in
                           (acc, arg' :: args))
       in
       F.ExprLocalCall {e with function_expr = f'; args = args'}
       |> return_expr is_top acc
    | ExprRemoteCall e ->
       let (acc, m') = extract_match_expr' acc false e.module_expr in
       let (acc, f') = extract_match_expr' acc false e.function_expr in
       (* same as ExprLocalCall comment *)
       let (acc, args') =
         List.fold_right e.args ~init:(acc, []) ~f:(fun arg (acc, args) ->
                           let (acc, arg') = extract_match_expr' acc false arg in
                           (acc, arg' :: args))
       in
       F.ExprRemoteCall {e with module_expr = m'; function_expr = f'; args = args'}
       |> return_expr is_top acc
    | ExprIf _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/224"];
                                                         message="support if"}))
    | ExprMapCreation e ->
       (* NOTE: Evaluation of match expressions in assocs does not affect each other.
        * e.g., `#{K1 = K2 => V1 = 1, K2 = 1 => V2 = V1}` cannot be compiled.
        * So we convert `#{K1 = k1 => V1 = v1, K2 = k2 => V2 = v2}` to
        * `K2 = k2, V2 = v2, K1 = k1, V1 = v1, #{k1 => v1, k2 => v2}` because it is
        * guaranteed by Erlang compiler that new variables introduced in an assoc
        * are not used in another assoc.
        *)
       let (acc, assocs') =
         List.fold_right e.assocs ~init:(acc, []) ~f:(fun assoc (acc, assocs) ->
                           let (acc, assoc') = extract_assoc acc assoc in
                           (acc, assoc' :: assocs))
       in
       F.ExprMapCreation {e with assocs = assocs'}
       |> return_expr is_top acc
    | ExprMapUpdate e ->
       let (acc, m') = extract_match_expr' acc false e.map in
       (* same as ExprMapCreation comment *)
       let (acc, assocs') =
         List.fold_right e.assocs ~init:(acc, []) ~f:(fun assoc (acc, assocs) ->
                           let (acc, assoc') = extract_assoc acc assoc in
                           (acc, assoc' :: assocs))
       in
       F.ExprMapUpdate {e with map = m'; assocs = assocs'}
       |> return_expr is_top acc
    | ExprMatch e ->
       let (acc, body') = extract_match_expr' acc false e.body in
       (* Make ExprMatch appeared to top in even if is_top=false *)
       (ExprMatch {e with body = body'} :: acc, body')
    | ExprBinOp e ->
       let (acc, lhs') = extract_match_expr' acc false e.lhs in
       let (acc, rhs') = extract_match_expr' acc false e.rhs in
       F.ExprBinOp {e with lhs = lhs'; rhs = rhs'}
       |> return_expr is_top acc
    | ExprUnaryOp e ->
       let (acc, operand') = extract_match_expr' acc false e.operand in
       F.ExprUnaryOp {e with operand = operand'}
       |> return_expr is_top acc
    | ExprReceive _ | ExprReceiveAfter _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/226"];
                                                         message="support receive"}))
    | ExprRecord _
      | ExprRecordFieldAccess _
      | ExprRecordFieldIndex _
      | ExprRecordUpdate _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/227"];
                                                         message="support record"}))
    | ExprTuple e ->
       (* NOTE: Evaluation of match expression in tuple elements does not affect each other.
        * e.g., `{A = 1, B = A}` cannot be compiled, and `{A = B, B = 1}` also.
        * So we convert `{A = 1, B = 1}` to `B = 1, A = 1, {1, 1}` because it is
        * guaranteed by Erlang compiler that new variables introduced in a tuple
        * element are not used in another element.
        *)
       let (acc, es') =
         List.fold_right e.elements ~init:(acc, []) ~f:(fun e (acc, es) ->
                           let (acc, e') = extract_match_expr' acc false e in
                           (acc, e' :: es))
       in
       F.ExprTuple {e with elements = es'}
       |> return_expr is_top acc
    | ExprTry _ ->
       raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/223"];
                                                         message="support try"}))
    | ExprVar _ as e -> return_expr is_top acc e
    | ExprLit _ as e -> return_expr is_top acc e
  and extract_assoc acc = function
    (* is_top=false because assocs are not appeared to top-level *)
    | F.ExprAssoc a ->
       let (acc, k') = extract_match_expr' acc false a.key in
       let (acc, v') = extract_match_expr' acc false a.value in
       (acc, F.ExprAssoc {a with key = k'; value = v'})
    | F.ExprAssocExact a ->
       let (acc, k') = extract_match_expr' acc false a.key in
       let (acc, v') = extract_match_expr' acc false a.value in
       (acc, F.ExprAssocExact {a with key = k'; value = v'})
  in
  let (es, _) = extract_match_expr' [] true e in List.rev es

let expr_of_atom_or_var = function
  | F.AtomVarAtom {line; atom} -> Constant (line, Atom atom)
  | AtomVarVar {line; id} -> Ref (line, Var id)

let expr_of_integer_or_var = function
  | F.IntegerVarInteger {line; integer} -> Constant (line, Number (Int integer))
  | IntegerVarVar {line; id} -> Ref (line, Var id)

let rec pattern_of_erlang_pattern = function
  | F.PatBitstr _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/228"];
                                                       message="support bitstr pattern"}))
  | F.PatCompound _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/229"];
                                                       message="support compound pattern"}))
  | F.PatBinOp _ | F.PatUnaryOp _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/230"];
                                                       message="support unary and binary operator pattern"}))
  | F.PatRecordFieldIndex _ | F.PatRecord _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/231"];
                                                       message="support record pattern"}))
  | F.PatVar {line; id} -> Ast.PatVar (line, id)
  | F.PatUniversal {line} -> Ast.PatVar (line, "_")
  | F.PatLit {lit} -> pattern_of_literal lit
  | F.PatMap {line; assocs} ->
     assocs
     |> List.map ~f:(fun (F.PatAssocExact {key; value; _}) -> (pattern_of_erlang_pattern key, pattern_of_erlang_pattern value))
     |> (fun assocs -> Ast.PatMap (line, assocs))
     | F.PatTuple {line; pats} ->
     PatTuple (line, (pats |> List.map ~f:pattern_of_erlang_pattern))
  | F.PatNil {line} -> PatNil line
  | F.PatCons {line; head; tail} ->
     PatCons (line, (pattern_of_erlang_pattern head, pattern_of_erlang_pattern tail))

let rec line_number_of_erlang_expr = function
  | F.ExprBody {exprs} -> line_number_of_erlang_expr (List.hd_exn exprs)
  | ExprBitstr {line; _} -> line
  | ExprBitstrComprehension {line; _} -> line
  | ExprBlock {line; _} -> line
  | ExprCase {line; _} -> line
  | ExprCatch {line; _} -> line
  | ExprCons {line; _} -> line
  | ExprNil {line} -> line
  | ExprListComprehension {line; _} -> line
  | ExprLocalFunRef {line; _} -> line
  | ExprRemoteFunRef {line; _} -> line
  | ExprFun {line; _} -> line
  | ExprLocalCall {line; _} -> line
  | ExprRemoteCall {line; _} -> line
  | ExprIf {line; _} -> line
  | ExprMapCreation {line; _} -> line
  | ExprMapUpdate {line; _} -> line
  | ExprMatch {line; _} -> line
  | ExprBinOp {line; _} -> line
  | ExprUnaryOp {line; _} -> line
  | ExprReceive {line; _} -> line
  | ExprReceiveAfter {line; _} -> line
  | ExprRecord {line; _} -> line
  | ExprRecordFieldAccess {line; _} -> line
  | ExprRecordFieldIndex {line; _} -> line
  | ExprRecordUpdate {line; _} -> line
  | ExprTuple {line; _} -> line
  | ExprTry {line; _} -> line
  | ExprVar {line; _} -> line
  | ExprLit {lit} ->
     match lit with
     | LitAtom {line; _} -> line
     | LitChar {line; _} -> line
     | LitFloat {line; _} -> line
     | LitInteger {line; _} -> line
     | LitBigInt {line; _} -> line
     | LitString {line; _} -> line

(* converts a secuence of expressions `[e1; e2; ...]` to an expression `let _ = e1 in let _ = e2 in ...` *)
(* assume `extract_toplevel` is applied to the argument *)
let rec expr_of_erlang_exprs = function
  | [] -> unit
  | [e] -> expr_of_erlang_expr' e
  | F.ExprMatch {line; pattern; body} :: es ->
     (* no match expression in `e` by extract_match_expr *)
     let body' = expr_of_erlang_expr' body in
     let es' = expr_of_erlang_exprs es in
     Case (line, body', [((pattern_of_erlang_pattern pattern, Constant (line, Atom "true")), es')])
  | e :: es ->
     Let (line_number_of_erlang_expr e, "_", expr_of_erlang_expr' e, expr_of_erlang_exprs es)
and expr_of_erlang_expr' = function
  | F.ExprBody {exprs} ->
     expr_of_erlang_exprs exprs
  | ExprBitstr _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/220"];
                                                       message="support bitstr expr"}))
  | ExprBitstrComprehension _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/221"];
                                                       message="support bitstr comprehension"}))
  | ExprBlock {exprs; _} ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/222"];
                                                       message="support block expr"}))
  | ExprCase {line; expr; clauses} ->
     let cs = clauses |> List.map ~f:(function
       | F.ClsCase {line; pattern; guard_sequence; body; _} ->
          if Option.is_some guard_sequence then Log.debug [%here] "line:%d %s" line "Guard (when clauses) are not supported";
          ((pattern_of_erlang_pattern pattern, Constant (line, Atom "true")), expr_of_erlang_expr' body)
       | F.ClsCatch _ | F.ClsFun _ | F.ClsIf _ ->
          failwith "cannot reach here"
    ) in
    Case (line, expr_of_erlang_expr' expr, cs)
  | ExprCatch {line; expr} ->
     let e = expr_of_erlang_expr' expr in
     Catch (line, e)
  | ExprLocalFunRef {line; function_name; arity} ->
     Ref(line, LocalFun {function_name; arity})
  | ExprRemoteFunRef {line; module_name; function_name; arity} ->
     let mfa =
       MFA {module_name = expr_of_atom_or_var module_name;
            function_name = expr_of_atom_or_var function_name;
            arity = expr_of_integer_or_var arity}
     in
     Ref (line, mfa)
  | ExprFun {line; name; clauses} ->
     let fun_abst = function_of_clauses' clauses in
     (* If name is omitted, don't create Letrec *)
     (match name with
     | Some name -> Letrec (line, [(Var name, fun_abst)], Ref (line, Var name))
     | None -> Abs (line, fun_abst)
     )
  | ExprLocalCall {line; function_expr=ExprLit {lit=LitAtom {atom=function_name; _}}; args} ->
     let arity = List.length args in
     let local_fun = LocalFun{function_name; arity} in
     App (line, Ref (line, local_fun), List.map ~f:expr_of_erlang_expr' args)
  | ExprLocalCall {line; function_expr; args} ->
     App (line, expr_of_erlang_expr' function_expr, List.map ~f:expr_of_erlang_expr' args)
  | ExprRemoteCall {line; module_expr; function_expr; args; _} ->
     let mfa = MFA {
       module_name=expr_of_erlang_expr' module_expr;
       function_name=expr_of_erlang_expr' function_expr;
       arity=Constant (line, Number (Int (List.length args)))}
     in
     App (line, Ref (line, mfa), List.map ~f:expr_of_erlang_expr' args)
  | ExprIf _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/224"];
                                                       message="support if expr"}))
  | ExprMatch {line; pattern; body} ->
     (* There is no match expression in `e` by `extract_match_expr`.
      * Futhermore, this match expression is single or the last of expr sequence because
      * a match expression which has subsequent expressions is caught in `expr_of_erlang_exprs`.
      * Therefore, we can put right-hand side expr of match expression to the return value of case expr.
      *)
     let e' = expr_of_erlang_expr' body in
     Case (line, e', [((pattern_of_erlang_pattern pattern, Constant (line, Atom "true")), e')])
  | ExprBinOp {line; op; lhs; rhs} ->
     let func = Ast.MFA {
        module_name = Constant (line, Atom "erlang");
        function_name = Constant (line, Atom op);
        arity=Constant (line, Number (Int 2))
     } in
     App(line, Ref (line, func), List.map ~f:expr_of_erlang_expr' [lhs; rhs])
  | ExprUnaryOp _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/225"];
                                                       message="support unary op"}))
  | ExprReceive _ | ExprReceiveAfter _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/226"];
                                                       message="support receive"}))
  | ExprRecord _ | ExprRecordFieldAccess _ | ExprRecordFieldIndex _ | ExprRecordUpdate _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/227"];
                                                       message="support record expr"}))
  | ExprTuple {line; elements} -> Tuple (line, List.map ~f:expr_of_erlang_expr' elements)
  | ExprTry _ ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/223"];
                                                       message="support try expr"}))
  | ExprVar {line; id} -> Ref (line, Var id)
  | ExprLit {lit} -> expr_of_literal lit
  | ExprCons {line; head; tail; _} -> ListCons (line, expr_of_erlang_expr' head, expr_of_erlang_expr' tail)
  | ExprNil {line} -> ListNil (line)
  | ExprListComprehension _ ->
     (* TODO: support list comprehension *)
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/92"]; message="support list comprehension `[E_0 || Q_1, ..., Q_k]`"}))
  | ExprMapCreation {line; assocs} ->
     assocs
     |> List.map ~f:(function
                     | F.ExprAssoc {key; value; _} -> (expr_of_erlang_expr' key, expr_of_erlang_expr' value)
                     | ExprAssocExact _ -> failwith "cannot reach here: map creation must not have exact assocs")
     |> (fun assocs -> MapCreation (line, assocs))
  | ExprMapUpdate {line; map; assocs} ->
     let assoc_divide assoc (assocs, exact_assocs) = match assoc with
       | F.ExprAssoc {key; value; _} ->
          ((expr_of_erlang_expr' key, expr_of_erlang_expr' value) :: assocs, exact_assocs)
       | ExprAssocExact {key; value; _} ->
          (assocs, (expr_of_erlang_expr' key, expr_of_erlang_expr' value) :: exact_assocs)
     in
     assocs
     |> List.fold_right ~init:([], []) ~f:assoc_divide
     |> (fun (assocs, exact_assocs) -> MapUpdate {line; map=expr_of_erlang_expr' map; assocs; exact_assocs})
and function_of_clauses' clauses =
    (* Create a list which have n elements *)
    let rec fill e = (function
    | 0 -> []
    | n -> (e())::(fill e (n - 1))
    )
    in
    let (cs, arities) = clauses |> List.map ~f:(function
    | F.ClsCase _ | F.ClsCatch _ | F.ClsIf _ -> failwith "cannot reach here"
    | F.ClsFun {line; patterns; body; _} ->
      (* Ignore guards currently since guard is complex and it's not needed for simple examples *)
      let ps = patterns |> List.map ~f:pattern_of_erlang_pattern in
      let arity = List.length ps in
      let tuple_pattern = PatTuple (line, ps) in
      (((tuple_pattern, Constant (line, Atom ("true"))), expr_of_erlang_expr' body), arity)
    ) |> List.unzip in
     let line_number_of_clause = function
     | ((PatTuple (_, _patterns), _), term) -> Ast.line_number_of_t term
     | ((PatConstant (_, _constant), _), term) -> Ast.line_number_of_t term
     | ((PatCons (_hd, _tl), _), term) -> Ast.line_number_of_t term
     | ((PatVar (_, _v), _), term) -> Ast.line_number_of_t term
     | ((PatNil _, _), term) -> Ast.line_number_of_t term
     | ((PatMap _, _), term) -> Ast.line_number_of_t term
     in
     let make_fresh_variables length = fill (fun () -> Variable.create()) length |> List.rev in
     let make_case cs fresh_variables =
       let line = line_number_of_clause (List.hd_exn cs) in
       let fresh_tuple = Tuple (
         line,
         fresh_variables |> List.map ~f:(fun v -> Ref (line, Var v))
       ) in
       (* letrec $name = fun $name(A1, A2, ...) -> b1; $name(B1, B2, ...)-> b2; ... end in $name *)
       Case (line, fresh_tuple, cs)
     in
     match cs with
     | ((PatTuple (_, patterns), Constant (_, Atom ("true"))), body)::[] ->
        let all_pattern_is_var = patterns |> List.for_all ~f:(function
       | PatVar (_, _) -> true
       | _ -> false
       ) in
       if all_pattern_is_var then
         let args = patterns |> List.map ~f:(function
         | PatVar (_line, v) -> v
         | _ -> failwith "cannot reach here"
       ) in
         {args; body}
       else
         let arity = List.length patterns in
         let fresh_variables: string list = (make_fresh_variables arity) in
         {args=fresh_variables; body=make_case cs fresh_variables}
     | cs ->
       (* Assume all arities have the same value *)
        let arity = List.nth_exn arities 0 in
        let fresh_variables = (make_fresh_variables arity) in
        {args=fresh_variables; body=make_case cs fresh_variables}

let expr_of_erlang_expr e = e |> extract_toplevel |> expr_of_erlang_expr'

let function_of_clauses clauses =
  List.map ~f:(function
    | F.ClsFun c ->
      F.ClsFun {c with body = extract_toplevel c.body}
    | _ ->
      failwith "cannot reach here")
    clauses
  |> function_of_clauses'

let forms_to_functions forms =
  let find_specs fun_name =
    List.find_map ~f:(function
                      | F.SpecFun {function_name; arity; specs; _} when fun_name = function_name ->
                         List.map ~f:(fun ty ->
                                    match Type.of_absform ty with
                                    | Type.(TyUnion [Type.TyFun (domains, range)]) -> (domains, range)
                                    | other -> failwith (!%"unexpected type spec of %s/%d: %s: %s" function_name arity (Type.pp other) (F.sexp_of_type_t ty |> Sexp.to_string_hum)))
                                  specs
                         |> Option.return
                      | _ -> None) forms
  in
  forms
  |> List.filter_map ~f:(function
                         | F.DeclFun {line; function_name; arity; clauses} ->
                            Some (line, function_name, arity, clauses)
                         | _ -> None)
  |> List.map ~f:(fun (_line, name, arity, clauses) ->
                let specs = find_specs name in
                let fun_abst = function_of_clauses clauses in
                {specs; fun_name=name; fun_abst})

let forms_to_module forms =
  let take_file forms =
    List.find_map ~f:(function F.AttrFile {line; file; file_line} -> Some (line, file, file_line) | _ -> None) forms
    |> Result.of_option ~error:(Failure "file attribute not found")
  in
  let take_module_name forms =
    List.find_map ~f:(function F.AttrMod {line; module_name} -> Some (line, module_name) | _ -> None) forms
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
