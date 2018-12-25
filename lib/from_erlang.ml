open Base
open Ast
open Constant
open Obeam
open Polymorphic_compare
module F = Abstract_format
open Common

let unit : expr = Constant (Number 0)

let const_of_literal = function
  | F.LitAtom (_line_t, name) -> Atom name
  | LitInteger (_line_t, i) -> Number i
  | LitBigInt (_line_t, z) ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/93"]; message="support bigint literal"}))
  | LitString (_line_t, s) ->
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/90"]; message="support string(list) literal"}))

(* [e1; e2; ...] という式の列を let _ = e1 in let _ = e2 ... in という１つの式にする *)
let rec expr_of_exprs = function
  | [] -> unit
  | [e] -> e
  | e :: es ->
     Let ("_", e, expr_of_exprs es)

let expr_of_atom_or_var = function
  | F.AtomVarAtom (_line_t, a) -> Constant (Atom a)
  | AtomVarVar (_line_t, v) -> Var v

let expr_of_integer_or_var = function
  | F.IntegerVarInteger (_line_t, i) -> Constant (Number i)
  | IntegerVarVar (_line_t, v) -> Var v

let rec pattern_of_erlang_pattern = function
  | F.PatVar (_, v) -> Ast.PatVar v
  | F.PatUniversal _ -> Ast.PatVar "_"
  | F.PatLit literal ->
     let _constant = const_of_literal literal in
     let issue_links = ["https://github.com/dwango/fialyzer/issues/103"] in
     let message = "support constant pattern" in
     raise Known_error.(FialyzerError (NotImplemented {issue_links; message}))
  | F.PatMap _ ->
     let issue_links = ["https://github.com/dwango/fialyzer/issues/102"] in
     let message = "support map pattern" in
     raise Known_error.(FialyzerError (NotImplemented {issue_links; message}))
  | F.PatTuple (_, patterns) ->
     PatTuple (patterns |> List.map ~f:pattern_of_erlang_pattern)

let rec expr_of_erlang_expr = function
  | F.ExprBody erlangs ->
     expr_of_exprs (List.map ~f:expr_of_erlang_expr erlangs)
  | ExprCase (line, e, clauses) ->
     let cs = clauses |> List.map ~f:(function
       | F.ClsCase (_, pattern, guard, e) ->
          if Option.is_some guard then Log.debug [%here] "line:%d %s" line "Guard (when clauses) are not supported";
          ((pattern_of_erlang_pattern pattern, Constant (Atom "true")), expr_of_erlang_expr e)
       | F.ClsFun (_, _, _, _) ->
          failwith "cannot reach here"
    ) in
    Case (expr_of_erlang_expr e, cs)
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
      (((tuple_pattern, Constant (Atom ("true"))), expr_of_erlang_expr body), arity)
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
     App (Var fun_name, List.map ~f:expr_of_erlang_expr args)
  | ExprLocalCall (_line_t, f, args) ->
     App (expr_of_erlang_expr f, List.map ~f:expr_of_erlang_expr args)
  | ExprRemoteCall (_line_t, _line_m, m, f, args) ->
     let mfa = MFA {
       module_name=expr_of_erlang_expr m;
       function_name=expr_of_erlang_expr f;
       arity=Constant (Number (List.length args))} in
     App (mfa, List.map ~f:expr_of_erlang_expr args)
  | ExprMatch (line_t, pat, e) ->
     (* TODO: support match expr `A = B` *)
     raise Known_error.(FialyzerError (NotImplemented {issue_links=["https://github.com/dwango/fialyzer/issues/81"]; message="support match expr `A = B`"}))
  | ExprBinOp (_line_t, op, e1, e2) ->
     let func = Ast.MFA {
        module_name = Constant (Atom "erlang");
        function_name = Constant (Atom op);
        arity=Constant (Number 2)
     } in
     App(func, List.map ~f:expr_of_erlang_expr [e1; e2])
  | ExprTuple (_line_t, es) ->
     Tuple (List.map ~f:expr_of_erlang_expr es)
  | ExprVar (_line_t, v) -> Var v
  | ExprLit literal -> Constant (const_of_literal literal)
  | ExprMapCreation (_, _) | ExprMapUpdate (_, _, _) -> failwith "not implemented"

let clauses_to_function = function
  | F.ClsCase(_line, _pattern, _guards, _body) ->
     failwith "not implemented: Clause Case"(* TODO : clause case *)
  | F.ClsFun(_line, args, _guards, body) ->
     let f = (function
     | F.PatVar (_, v) -> v
     | F.PatUniversal _ -> "_"
     | F.PatMap (_, _) | F.PatLit _ | F.PatTuple (_, _) -> failwith "not implemented"
     ) in
     let vs = args |> List.map ~f:f in
     (vs, expr_of_erlang_expr body)

let rec typ_of_erlang_type = function
(*  | F.TyAnn (_line, _, _) ->
  | TyBitstring ->
 *)
  | F.TyPredef (_line, "number", []) -> Type.TyNumber
     (*
  | TyProduct ->
  | TyBinOp ->
  | TyUnaryOp ->
  | TyAnyMap ->
  | TyMap ->
 *)
  | F.TyVar (_line, v) -> Type.TyVar (Type_variable.of_string v)
(*  | TyContFun ->
 *)
  | TyFun (_line, TyProduct (_, args), range) ->
     Type.TyFun(List.map ~f:typ_of_erlang_type args, typ_of_erlang_type range)
(*
  | TyAnyTuple ->
  | TyTuple ->
  | TyUnion ->
  | TyUser ->
 *)
  | TyLit (LitAtom (_, atom)) -> TySingleton (Atom atom)
  | other ->
     Log.debug [%here] "not implemented type: %s" (F.sexp_of_type_t other |> Sexp.to_string_hum);
     Type.TyAny

let forms_to_functions forms =
  let find_specs fun_name =
    List.find_map ~f:(function
                      | F.SpecFun (_line, _mod_name, fname, arity, specs) when fun_name = fname ->
                         List.map ~f:(fun ty ->
                                    match typ_of_erlang_type ty with
                                    | Type.TyFun (domains, range) -> (domains, range)
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
