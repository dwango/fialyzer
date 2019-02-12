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
  let ctx = Context.create ~import_modules in (*TODO: https://github.com/dwango/fialyzer/issues/167 *)
  let open Result in
  result_map_m ~f:(check_module plt ctx) modules >>= fun _ ->
  Result.return ()
