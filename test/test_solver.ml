open Base
open Fialyzer
open Ast_intf
open Solver

let%expect_test "meet" =
  let print ty1 ty2 =
    meet init ty1 ty2
    |> [%sexp_of: typ]
    |> Expect_test_helpers_kernel.print_s in

  print TyAny TyAny;
  [%expect {| TyAny |}];

  print TyNone TyAny;
  [%expect {| TyNone |}];

  print (TyConstant (Int 1)) (TyConstant (Int 2));
  [%expect {| TyNone |}];

  print (TyUnion (TyUnion (TyConstant (Int 1), TyConstant (Int 2)), TyConstant (Int 3)))
        (TyUnion (TyUnion (TyConstant (Int 2), TyConstant (Int 3)), TyConstant (Int 4)));
  (* TODO: normalize *)
  [%expect {|
    (TyUnion
      (TyUnion
        (TyUnion (TyUnion TyNone TyNone) TyNone)
        (TyUnion (TyUnion (TyConstant (Int 2)) TyNone) TyNone))
      (TyUnion (TyUnion TyNone (TyConstant (Int 3))) TyNone)) |}];

  print (TyUnion (TyUnion (TyConstant (Int 1), TyConstant (Int 2)), TyConstant (Int 3))) TyInteger;
  [%expect {|
    (TyUnion
      (TyUnion
        (TyConstant (Int 1))
        (TyConstant (Int 2)))
      (TyConstant (Int 3))) |}]

let%expect_test "solver" =
  let print c =
    solve init c
    |> [%sexp_of: (solution, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  let create_vars n =
    Type_variable.reset_count ();
    List.init n ~f:(fun _ -> Type_variable.create ()) in

  print Empty;
  [%expect {| (Ok ()) |}];

  let [a] = create_vars 1 in
  print (Eq (TyVar a, TyInteger));
  [%expect {| (Ok ((a TyInteger))) |}];

  let [a] = create_vars 1 in
  print (Subtype (TyVar a, TyInteger));
  [%expect {| (Ok ((a TyInteger))) |}];
