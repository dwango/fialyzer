open Base
open Common
open Poly

let expr_of_module m =
  let open Ast in
  let funs =
    m.functions
    |> List.map ~f:(fun {specs; fun_name; fun_abst} ->
                  let arity = List.length fun_abst.args in
                  (LocalFun {function_name=fun_name; arity}, fun_abst))
  in
  let body =
    m.functions
    |> List.map ~f:(fun {specs; fun_name=name; fun_abst={args; body}} ->
                  Tuple (-1, [Constant (-1, Atom name);
                              Ref (-1, LocalFun {function_name=name; arity=List.length args})]))
    |> (fun es -> Tuple (-1, es))
  in
  Letrec(-1, funs, body)
  |> Result.return

(* return success types of functions *)
let function_types sol ty =
  let open Type in
  let atom_of_ty = function
    | TyUnion [TySingleton (Atom atom)] -> atom
    | _ -> failwith "cannot reach here"
  in
  let fun_type ty =
    match ty with
    | TyUnion [TyTuple [ty_fun_name; fun_type]] ->
       (atom_of_ty ty_fun_name, Solver.lookup_type sol fun_type)
    | _ -> failwith "cannot reach here"
  in
  match ty with
  | TyUnion [TyTuple tys] ->
     List.map ~f:fun_type tys
  | _ -> failwith "cannot reach here"

let check_module_spec module_ infered_types =
  let module_name = module_.Ast.name in
  let specs = Ast.specs_of_module module_ in
  let check_function_spec f =
    let function_name = f.Ast.fun_name in
    let arity = List.length f.Ast.fun_abst.args in
    let infered_type = List.Assoc.find_exn infered_types ~equal:(=) f.Ast.fun_name in
    let expected_type =
      let mfa = Mfa.{module_name; function_name; arity} in
      match List.Assoc.find specs ~equal:(=) mfa with
      | None -> Type.TyAny
      | Some spec -> spec
    in
    match Type.inf expected_type infered_type with
    | Type.TyBottom ->
       let line = Ast.line_number_of_t f.Ast.fun_abst.body in
       Known_error.{
         filename=module_.Ast.file; module_name; line;
         function_name; type_spec=expected_type; success_type=infered_type;
       }
       |> Result.fail
    | _ ->

       Ok ()
  in
  module_.Ast.functions
  |> List.filter_map ~f:(check_function_spec >>> Result.error)
  |> function
    | [] -> Ok ()
    | errors ->
       Error Known_error.(FialyzerError (TypeSpecUnmatch errors))



let check_module plt ctx m =
  let open Result in
  Log.debug [%here] "Checking module: %s" (m.Ast.name);
  expr_of_module m >>= fun expr ->
  Derivation.derive ctx expr >>= fun (ty, c) ->
  Log.debug [%here] "Constraints:\n%s" (Constraint.show c);
  Solver.solve Solver.init c >>= fun sol ->
  Log.debug [%here] "Types:\n%s" (Solver.lookup_type sol ty |> Type.pp);
  check_module_spec m (function_types sol ty) >>= fun _ ->
  Ok ()

let check_modules plt modules =
  let import_modules = ["erlang"] in (*TODO: https://github.com/dwango/fialyzer/issues/166 *)
  let specs =
    modules
    |> List.concat_map ~f:Ast.specs_of_module
  in
  let ctx0 = Context.create ~import_modules plt in (*TODO: https://github.com/dwango/fialyzer/issues/167 *)
  let ctx =
    List.fold_left ~f:(fun ctx (mfa, ty) ->
			   Context.add (Context.Key.MFA mfa) ty ctx)
                   ~init:ctx0 specs
  in
  let open Result in
  result_map_m ~f:(check_module plt ctx) modules >>= fun _ ->
  Result.return ()
