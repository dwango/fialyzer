open Base
open Ast_intf
open Derivation

let%expect_test "derivation" =
  let print context term =
    Type_variable.reset_count ();
    derive context term
    |> [%sexp_of: (typ * constraint_, string) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  print Context.empty (Val (Int 42));
  [%expect {| (Ok ((TyConstant (Int 42)) Empty)) |}];

  print Context.empty (Var "x");
  [%expect {| (Error "unknown type variable: x") |}];

  print (Context.add "x" TyInteger Context.empty) (Var "x");
  [%expect {| (Ok (TyInteger Empty)) |}];

  print Context.empty (Abs (["x"], Var "x"));
  [%expect {|
    (Ok (
      (TyVar b) (Eq (TyVar b) (TyConstraint (TyFun ((TyVar a)) (TyVar a)) Empty)))) |}];

  print Context.empty (Abs (["x"; "y"; "z"], Var "x"));
  [%expect {|
    (Ok (
      (TyVar d)
      (Eq
        (TyVar d)
        (TyConstraint
          (TyFun
            ((TyVar a)
             (TyVar b)
             (TyVar c))
            (TyVar a))
          Empty)))) |}];

  print Context.empty (App (Abs (["x"], Var "x"), [Val (Int 42)]));
  [%expect {|
    (Ok (
      (TyVar e)
      (Conj (
        (Eq (TyVar b) (TyFun ((TyVar c)) (TyVar d)))
        (Subtype
          (TyVar e)
          (TyVar d))
        (Subtype (TyConstant (Int 42)) (TyVar c))
        (Eq (TyVar b) (TyConstraint (TyFun ((TyVar a)) (TyVar a)) Empty))
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
  [%expect {| (Ok ((TyVar a) (Conj (Empty (Eq (TyVar a) (TyConstant (Int 42))) Empty)))) |}];

  print
    Context.empty
    (Letrec
      ([
        ("f", Abs (["x"], App (Var "g", [Var "x"])));
        ("g", Abs (["x"], App (Var "f", [Var "x"])))
      ], App (Var "f", [Val (Int 42)])));
  [%expect {|
    (Ok (
      (TyVar o)
      (Conj (
        (Conj (
          (Eq (TyVar a) (TyFun ((TyVar m)) (TyVar n)))
          (Subtype
            (TyVar o)
            (TyVar n))
          (Subtype (TyConstant (Int 42)) (TyVar m))
          Empty
          Empty))
        (Eq
          (TyVar a)
          (TyVar g))
        (Eq
          (TyVar g)
          (TyConstraint
            (TyFun ((TyVar c)) (TyVar f))
            (Conj (
              (Eq (TyVar b) (TyFun ((TyVar d)) (TyVar e)))
              (Subtype (TyVar f) (TyVar e))
              (Subtype (TyVar c) (TyVar d))
              Empty
              Empty))))
        (Eq
          (TyVar b)
          (TyVar l))
        (Eq
          (TyVar l)
          (TyConstraint
            (TyFun ((TyVar h)) (TyVar k))
            (Conj (
              (Eq (TyVar a) (TyFun ((TyVar i)) (TyVar j)))
              (Subtype (TyVar k) (TyVar j))
              (Subtype (TyVar h) (TyVar i))
              Empty
              Empty)))))))) |}];
