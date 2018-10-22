open Base
open Ast_intf
open Result

let tyvar_count = ref 0

let new_tyvar () =
  Int.incr tyvar_count;
  TyVar (Printf.sprintf "%s%02d" "v" !tyvar_count)

let rec derive context = function
  | Val c ->
     Ok (TyConstant c, Empty)
  | Var v ->
     begin match Context.find context v with
     | Some ty ->
        Ok (ty, Empty)
     | None ->
        Error ("unknown type variable: " ^ v)
     end
  (* TODO: implement derivation of application expression with multiple arguments *)
  | App (f, [arg1]) ->
     derive context f >>= fun (tyf, cf) ->
     derive context arg1 >>= fun (ty1, c1) ->
     let alpha1 = new_tyvar () in
     let alpha = new_tyvar () in
     let beta = new_tyvar () in
     let constraints = [
         Eq (tyf, TyFun ([alpha1], alpha));
         Subtype (beta, alpha);
         Subtype (ty1, alpha1);
         cf;
         c1;
       ]
     in
     Ok (beta, Conj constraints)
  | Abs (vs, e) ->
     let new_tyvars = List.map ~f:(fun v -> (v, (new_tyvar ()))) vs in
     let added_context =
       List.fold_left
         ~f:(fun context (v, tyvar) -> Context.add v tyvar context)
         ~init:context
         new_tyvars in
     derive added_context e >>= fun (ty_e, c) ->
     let tyvar = new_tyvar () in
     Ok (tyvar, Eq (tyvar, TyConstraint (TyFun (List.map ~f:snd new_tyvars, ty_e), c)))
  | Let (v, e1, e2) ->
     derive context e1 >>= fun (ty_e1, c1) ->
     derive (Context.add v ty_e1 context) e2 >>= fun (ty_e2, c2) ->
     Ok (ty_e2, Conj [c1; c2])
  | Letrec (lets , e) ->
     let new_tyvars = List.map ~f:(fun (v, f) -> (v, f, (new_tyvar ()))) lets in
     let added_context =
       List.fold_left
         ~f:(fun context (v, _, tyvar) -> Context.add v tyvar context)
         ~init:context
         new_tyvars in
     let constraints_result =
       List.fold_left
         ~f:(fun constraints_result (v, f, tyvar) ->
               constraints_result >>= fun constraints ->
               derive added_context f >>= fun (ty, c) ->
               Ok (Eq (tyvar, ty) :: c :: constraints))
         ~init:(Ok [])
         new_tyvars in
     constraints_result >>= fun constraints ->
     derive added_context e >>= fun (ty, c) ->
     Ok (ty, Conj (c :: constraints))
  | other ->
     Error (Printf.sprintf "unsupported type: %s" (show_expr other))

let%expect_test "derivation" =
  let print context term =
    tyvar_count := 0;
    Expect_test_helpers_kernel.print_s
      [%sexp ((derive context term) : (typ * constraint_, string) Result.t)] in

  print Context.empty (Val (Int 42));
  [%expect {| (Ok ((TyConstant (Int 42)) Empty)) |}];

  print Context.empty (Var "x");
  [%expect {| (Error "unknown type variable: x") |}];

  print (Context.add "x" TyInteger Context.empty) (Var "x");
  [%expect {| (Ok (TyInteger Empty)) |}];

  print Context.empty (Abs (["x"], Var "x"));
  [%expect {|
    (Ok (
      (TyVar v02)
      (Eq (TyVar v02) (TyConstraint (TyFun ((TyVar v01)) (TyVar v01)) Empty)))) |}];

  print Context.empty (Abs (["x"; "y"; "z"], Var "x"));
  [%expect {|
    (Ok (
      (TyVar v04)
      (Eq
        (TyVar v04)
        (TyConstraint
          (TyFun
            ((TyVar v01)
             (TyVar v02)
             (TyVar v03))
            (TyVar v01))
          Empty)))) |}];

  print Context.empty (App (Abs (["x"], Var "x"), [Val (Int 42)]));
  [%expect {|
    (Ok (
      (TyVar v05)
      (Conj (
        (Eq (TyVar v02) (TyFun ((TyVar v03)) (TyVar v04)))
        (Subtype
          (TyVar v05)
          (TyVar v04))
        (Subtype (TyConstant (Int 42)) (TyVar v03))
        (Eq (TyVar v02) (TyConstraint (TyFun ((TyVar v01)) (TyVar v01)) Empty))
        Empty)))) |}];

  (* TODO: implement derivation of application expression with multiple arguments
  print Context.empty (App (Abs (["x"; "y"], Var "x"), [Val (Int 42); Val (Int 57)]));
  [%expect {| |}];
  *)

  print Context.empty (Let ("x", Val (Int 42), Var "x"));
  [%expect {|
    (Ok (
      (TyConstant (Int   42))
      (Conj       (Empty Empty)))) |}];

  print Context.empty (Letrec ([("x", Val (Int 42))], Var "x"));
  [%expect {| (Ok ((TyVar v01) (Conj (Empty (Eq (TyVar v01) (TyConstant (Int 42))) Empty)))) |}];

  print
    Context.empty
    (Letrec
      ([
        ("f", Abs (["x"], App (Var "g", [Var "x"])));
        ("g", Abs (["x"], App (Var "f", [Var "x"])))
      ], App (Var "f", [Val (Int 42)])));
  [%expect {|
    (Ok (
      (TyVar v15)
      (Conj (
        (Conj (
          (Eq (TyVar v01) (TyFun ((TyVar v13)) (TyVar v14)))
          (Subtype
            (TyVar v15)
            (TyVar v14))
          (Subtype (TyConstant (Int 42)) (TyVar v13))
          Empty
          Empty))
        (Eq
          (TyVar v02)
          (TyVar v12))
        (Eq
          (TyVar v12)
          (TyConstraint
            (TyFun ((TyVar v08)) (TyVar v11))
            (Conj (
              (Eq (TyVar v01) (TyFun ((TyVar v09)) (TyVar v10)))
              (Subtype (TyVar v11) (TyVar v10))
              (Subtype (TyVar v08) (TyVar v09))
              Empty
              Empty))))
        (Eq
          (TyVar v01)
          (TyVar v07))
        (Eq
          (TyVar v07)
          (TyConstraint
            (TyFun ((TyVar v03)) (TyVar v06))
            (Conj (
              (Eq (TyVar v02) (TyFun ((TyVar v04)) (TyVar v05)))
              (Subtype (TyVar v06) (TyVar v05))
              (Subtype (TyVar v03) (TyVar v04))
              Empty
              Empty)))))))) |}];
