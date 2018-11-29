open Base
open Fialyzer
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

  print Context.empty (Tuple [Val (Int 42); Val (Atom "x")]);
  [%expect {|
    (Ok (
      (TyTuple (
        (TyConstant (Int  42))
        (TyConstant (Atom x))))
      (Conj (Empty Empty))))
  |}];

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

  print Context.empty (App (Val (Int 57), [Val (Int 42)]));
  [%expect {|
    (Ok (
      (TyVar c)
      (Conj (
        (Eq (TyConstant (Int 57)) (TyFun ((TyVar a)) (TyVar b)))
        (Subtype
          (TyVar c)
          (TyVar b))
        Empty
        (Subtype (TyConstant (Int 42)) (TyVar a))
        Empty)))) |}];

  print Context.empty (App (Val (String "I am a function!"), [Val (Int 42); Val (Int 57)]));
  [%expect {|
    (Ok (
      (TyVar d)
      (Conj (
        (Eq
          (TyConstant (String "I am a function!"))
          (TyFun
            ((TyVar a)
             (TyVar b))
            (TyVar c)))
        (Subtype
          (TyVar d)
          (TyVar c))
        Empty
        (Subtype (TyConstant (Int 42)) (TyVar a))
        Empty
        (Subtype (TyConstant (Int 57)) (TyVar b))
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
        (Eq (TyVar b) (TyConstraint (TyFun ((TyVar a)) (TyVar a)) Empty))
        (Subtype (TyConstant (Int 42)) (TyVar c))
        Empty)))) |}];

  print Context.empty (App (Abs (["x"; "y"], Var "x"), [Val (Int 42); Val (Int 57)]));
  [%expect {|
    (Ok (
      (TyVar g)
      (Conj (
        (Eq
          (TyVar c)
          (TyFun
            ((TyVar d)
             (TyVar e))
            (TyVar f)))
        (Subtype
          (TyVar g)
          (TyVar f))
        (Eq
          (TyVar c)
          (TyConstraint
            (TyFun
              ((TyVar a)
               (TyVar b))
              (TyVar a))
            Empty))
        (Subtype (TyConstant (Int 42)) (TyVar d))
        Empty
        (Subtype (TyConstant (Int 57)) (TyVar e))
        Empty)))) |}];

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
          Empty
          (Subtype (TyConstant (Int 42)) (TyVar m))
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
              (Subtype
                (TyVar f)
                (TyVar e))
              Empty
              (Subtype
                (TyVar c)
                (TyVar d))
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
              (Subtype
                (TyVar k)
                (TyVar j))
              Empty
              (Subtype
                (TyVar h)
                (TyVar i))
              Empty)))))))) |}];
