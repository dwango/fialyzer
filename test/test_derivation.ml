open Base
open Ast_intf
open Derivation

let%expect_test "derivation" =
  let print context term =
    reset_tyvar_count ();
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
