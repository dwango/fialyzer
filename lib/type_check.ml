open Base
open Common

let check_module plt ctx m =
  let open Result in
  Log.debug [%here] "Checking module: %s" (m.Ast.name);
  From_erlang.module_to_expr m >>= fun expr ->
  Derivation.derive ctx expr >>= fun (ty, c) ->
  Log.debug [%here] "Constraints:\n%s" (Constraint.show c);
  Solver.solve Solver.init c >>= fun sol ->
  Log.debug [%here] "Types:\n%s" (Solver.lookup_type sol ty |> Type.pp);
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
