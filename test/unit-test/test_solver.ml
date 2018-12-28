open Base
open Fialyzer
open Type
open Solver

let sexp_of_solution sol =
  Map.map ~f:pp sol
  |> [%sexp_of: string Map.M(Type_variable).t]

let%expect_test "solver" =
  let print c =
    solve init c
    |> Result.map ~f:sexp_of_solution
    |> [%sexp_of: (Sexp.t, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  let create_vars n =
    Type_variable.reset_count ();
    List.init n ~f:(fun _ -> Type_variable.create ()) |> List.rev in

  print Empty;
  [%expect {| (Ok ()) |}];

  let [a] = create_vars 1 in
  print (Eq (Type.of_elem (TyVar a), Type.of_elem TyNumber));
  [%expect {| (Ok ((a "number()"))) |}];

  let [a] = create_vars 1 in
  print (Subtype (Type.of_elem (TyVar a), Type.of_elem TyNumber));
  [%expect {| (Ok ((a "number()"))) |}];

  let [a; b] = create_vars 2 in
  print (Subtype
           (Type.of_elem (TyTuple [Type.of_elem (TyVar a); Type.of_elem (TyVar b)]),
            Type.of_elem (TyTuple [Type.of_elem TyNumber; Type.of_elem TyAtom])));
  [%expect {|
    (Ok (
      (a "number()")
      (b "atom()"))) |}];
(*
  let [a; b] = create_vars 2 in
  print (Subtype (TyUnion [TyVar a; TyVar b], TyNumber));
  [%expect {|
    (Ok (
      (a TyNumber)
      (b TyNumber))) |}];
 *)
  print (Subtype (TyBottom, TyAny));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyAny, TyAny));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyBottom, TyAny));
  [%expect {| (Ok ()) |}];

  print (Subtype (TyBottom, TyBottom));
  [%expect {| (Ok ()) |}];

  print (Subtype (Type.of_elem TyNumber, Type.of_elem TyAtom));
  [%expect {| (Error ("Fialyzer.Known_error.FialyzerError(_)")) |}];
