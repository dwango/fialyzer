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

  print TyBottom TyAny;
  [%expect {| TyBottom |}];

  print (TySingleton (Number 1)) (TySingleton (Number 2));
  [%expect {| TyBottom |}];

  print (TyUnion (TyUnion (TySingleton (Number 1), TySingleton (Number 2)), TySingleton (Number 3)))
        (TyUnion (TyUnion (TySingleton (Number 2), TySingleton (Number 3)), TySingleton (Number 4)));
  (* TODO: normalize *)
  [%expect {|
    (TyUnion
      (TyUnion
        (TyUnion (TyUnion TyBottom TyBottom) TyBottom)
        (TyUnion (TyUnion (TySingleton (Number 2)) TyBottom) TyBottom))
      (TyUnion (TyUnion TyBottom (TySingleton (Number 3))) TyBottom)) |}];

  print (TyUnion (TyUnion (TySingleton (Number 1), TySingleton (Number 2)), TySingleton (Number 3))) TyNumber;
  [%expect {|
    (TyUnion
      (TyUnion
        (TySingleton (Number 1))
        (TySingleton (Number 2)))
      (TySingleton (Number 3))) |}]

let%expect_test "solver" =
  let print c =
    solve init c
    |> [%sexp_of: (solution, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  let create_vars n =
    Type_variable.reset_count ();
    List.init n ~f:(fun _ -> Type_variable.create ()) |> List.rev in

  print Empty;
  [%expect {| (Ok ()) |}];

  let [a] = create_vars 1 in
  print (Eq (TyVar a, TyNumber));
  [%expect {| (Ok ((a TyNumber))) |}];

  let [a] = create_vars 1 in
  print (Subtype (TyVar a, TyNumber));
  [%expect {| (Ok ((a TyNumber))) |}];

  let [a; b] = create_vars 2 in
  print (Subtype (TyTuple [TyVar a; TyVar b], TyTuple [TyNumber; TyAtom]));
  [%expect {|
    (Ok (
      (a TyNumber)
      (b TyAtom))) |}];

  let [a; b] = create_vars 2 in
  print (Subtype (TyUnion (TyVar a, TyVar b), TyNumber));
  [%expect {|
    (Ok (
      (a TyNumber)
      (b TyNumber))) |}];

  print (Subtype (TyBottom, TyAny));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyAny, TyAny));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyBottom, TyAny));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyBottom, TyBottom));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyNumber, TyAtom));
  [%expect {| (Error (Failure "there is no solution that satisfies subtype constraints")) |}];
