open Base

let new_var =
  let i = ref 0 in
  fun () ->
    Int.incr i;
    Printf.sprintf "%s%02d" "v" !i

let rec derive context = function
  | Types.Val c ->
     Ok (Types.TyConstant c, Types.Empty)
  | Types.Var v ->
     begin match Context.find context v with
     | Some ty ->
        Ok(ty, Types.Empty)
     | None ->
        Error ("unknown type variable: " ^ v)
     end
  | Types.App (f, [arg1]) ->
     let open Result in
     derive context f >>= fun (tyf, cf) ->
     derive context arg1 >>= fun (ty1, c1) ->
     begin match tyf with
     | Types.Fun([ty_arg1], ty) ->
        let beta = new_var() in
        Ok(Types.TyVar beta, Types.Conj [cf; c1])
     | _ ->
        Error "expected function type"
     end
  | Types.Abs ([v], e) ->
     let open Result in
     let ty_v = new_var () in
     derive (Context.add v (Types.TyVar ty_v) context) e >>= fun (ty_e, c) ->
     Ok(Types.Fun([Types.TyVar ty_v], Types.Constraint(ty_e, c)), Types.Empty)
  | other ->
     Error (Printf.sprintf "unsupported type: %s" (Types.show_expr other))
