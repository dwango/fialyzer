open Base
open Common

let check_module plt ctx m =
  let open Result in
  Log.debug [%here] "Checking module: %s" (m.Ast.name);
  From_erlang.module_to_expr m >>= fun expr ->
  Derivation.derive (Context.init ()) expr >>= fun (ty, c) ->
  Log.debug [%here] "Constraints:\n%s" (Type.show_constraint c);
  Solver.solve Solver.init c >>= fun sol ->
  Log.debug [%here] "Types:\n%s" (Solver.lookup_type sol ty |> Type.pp);
  Ok ()

let check_modules plt modules =
  let ctx = Context.init () in (*TODO: make type context from specs of the modules *)
  let open Result in
  result_map_m ~f:(check_module plt ctx) modules >>= fun _ ->
  Result.return ()
