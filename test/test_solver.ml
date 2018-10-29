open Base
open Fialyzer
open Ast_intf
open Solver

let%expect_test "meet" =
  let print ty1 ty2 =
    meet ty1 ty2
    |> [%sexp_of: (typ, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  print TyAny TyAny;
  [%expect {| (Ok TyAny) |}];

  print (TyConstant (Int 1)) (TyConstant (Int 2));
  [%expect {| (Ok TyNone) |}]

let%expect_test "solver" =
  let print c =
    solve init c
    |> [%sexp_of: (sol, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  let create_vars =
    Type_variable.reset_count ();
    List.init ~f:(fun _ -> Type_variable.create ()) in

  print Empty;
  [%expect {| (Ok ()) |}];

  let [a] = create_vars 1 in
  print (Eq (TyVar a, TyInteger));
  [%expect {| (Ok ((a TyInteger))) |}];

  let [a] = create_vars 1 in
  print (Subtype (TyVar a, TyInteger));
  [%expect {| (Ok ((a TyInteger))) |}];

  let [a; b] = create_vars 2 in
  print (Subtype (TyStruct [TyVar a; TyInteger], TyStruct [TyAtom; TyVar b]));
  [%expect {| (Ok ((a TyAtom) (b TyAny))) |}];

  let [a] = create_vars 1 in
  print (Subtype (TyVar a, TyUnion (TyInteger, TyAtom)))
