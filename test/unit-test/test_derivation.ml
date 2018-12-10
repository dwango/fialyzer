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

  print Context.empty (Constant (Number 42));
  [%expect {| (Ok ((TySingleton (Number 42)) Empty)) |}];

  print Context.empty (Var "x");
  [%expect {| (Error "unknown type variable: x") |}];

  print (Context.add (Context.Key.Var "x") TyNumber Context.empty) (Var "x");
  [%expect {| (Ok (TyNumber Empty)) |}];

  print Context.empty (Tuple [Constant (Number 42); Constant (Atom "x")]);
  [%expect {|
    (Ok (
      (TyTuple (
        (TySingleton (Number 42))
        (TySingleton (Atom   x))))
      (Conj (Empty Empty))))
  |}];

  (*
   * case 42 of
   *   X when true -> X
   * end
   *)
  print Context.empty (Case (Constant (Number 42), [(PatVar "X", Constant (Atom "true")), Var "X"]));
  [%expect {|
     (Ok (
       (TyVar a)
       (Conj (
         (Disj ((
           Conj (
             (Subtype
               (TySingleton (Atom true))
               (TyUnion
                 (TySingleton (Atom true))
                 (TySingleton (Atom false))))
             (Eq
               (TyVar a)
               (TyVar b))
             (Eq (TySingleton (Number 42)) (TyVar b))
             Empty
             Empty
             Empty))))
         Empty))))
  |}];


  (*
   * case {41, 42} of
   *   {X, Y} when true -> {X, Y}
   * end
   *)
  print Context.empty (Case (Tuple [Constant (Number 41); Constant (Number 42)], [(PatTuple [PatVar "X"; PatVar "Y"], Constant (Atom "true")), Tuple [Var "X"; Var "Y"]]));
  [%expect {|
     (Ok (
       (TyVar a)
       (Conj (
         (Disj ((
           Conj (
             (Subtype
               (TySingleton (Atom true))
               (TyUnion
                 (TySingleton (Atom true))
                 (TySingleton (Atom false))))
             (Eq
               (TyVar a)
               (TyTuple (
                 (TyVar b)
                 (TyVar c))))
             (Eq
               (TyTuple (
                 (TySingleton (Number 41))
                 (TySingleton (Number 42))))
               (TyTuple (
                 (TyVar b)
                 (TyVar c))))
             (Conj (Empty Empty))
             Empty
             (Conj (Empty Empty))))))
         (Conj (Empty Empty))))))
  |}];

  (*
   * case 42 of
   *   X when false -> X;
   *   X when true -> X
   * end
   *)
  print Context.empty (Case 
    (Constant (Number 42), 
      [(PatVar "X", Constant (Atom "false")), Var "X";
       (PatVar "X", Constant (Atom "true")), Var "X"]));
  [%expect {|
     (Ok (
       (TyVar a)
       (Conj (
         (Disj (
           (Conj (
             (Subtype
               (TySingleton (Atom false))
               (TyUnion
                 (TySingleton (Atom true))
                 (TySingleton (Atom false))))
             (Eq
               (TyVar a)
               (TyVar b))
             (Eq (TySingleton (Number 42)) (TyVar b))
             Empty
             Empty
             Empty))
           (Conj (
             (Subtype
               (TySingleton (Atom true))
               (TyUnion
                 (TySingleton (Atom true))
                 (TySingleton (Atom false))))
             (Eq
               (TyVar a)
               (TyVar c))
             (Eq (TySingleton (Number 42)) (TyVar c))
             Empty
             Empty
             Empty))))
         Empty))))
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
          Empty)))) 
   |}];

  print Context.empty (App (Constant (Number 57), [Constant (Number 42)]));
  [%expect {|
    (Ok (
      (TyVar c)
      (Conj (
        (Eq (TySingleton (Number 57)) (TyFun ((TyVar a)) (TyVar b)))
        (Subtype
          (TyVar c)
          (TyVar b))
        Empty
        (Subtype (TySingleton (Number 42)) (TyVar a))
        Empty)))) |}];

  print Context.empty (App (Constant (Atom "I am a function!"), [Constant (Number 42); Constant (Number 57)]));
  [%expect {|
    (Ok (
      (TyVar d)
      (Conj (
        (Eq
          (TySingleton (Atom "I am a function!"))
          (TyFun
            ((TyVar a)
             (TyVar b))
            (TyVar c)))
        (Subtype
          (TyVar d)
          (TyVar c))
        Empty
        (Subtype (TySingleton (Number 42)) (TyVar a))
        Empty
        (Subtype (TySingleton (Number 57)) (TyVar b))
        Empty)))) |}];

  print Context.empty (App (Abs (["x"], Var "x"), [Constant (Number 42)]));
  [%expect {|
    (Ok (
      (TyVar e)
      (Conj (
        (Eq (TyVar b) (TyFun ((TyVar c)) (TyVar d)))
        (Subtype
          (TyVar e)
          (TyVar d))
        (Eq (TyVar b) (TyConstraint (TyFun ((TyVar a)) (TyVar a)) Empty))
        (Subtype (TySingleton (Number 42)) (TyVar c))
        Empty)))) |}];

  print Context.empty (App (Abs (["x"; "y"], Var "x"), [Constant (Number 42); Constant (Number 57)]));
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
        (Subtype (TySingleton (Number 42)) (TyVar d))
        Empty
        (Subtype (TySingleton (Number 57)) (TyVar e))
        Empty)))) |}];

  print Context.empty (Let ("x", Constant (Number 42), Var "x"));
  [%expect {|
    (Ok (
      (TySingleton (Number 42))
      (Conj        (Empty  Empty)))) |}];

  print Context.empty (Letrec ([("x", Constant (Number 42))], Var "x"));
  [%expect {| (Ok ((TyVar a) (Conj (Empty (Eq (TyVar a) (TySingleton (Number 42))) Empty)))) |}];

  print
    Context.empty
    (Letrec
      ([
        ("f", Abs (["x"], App (Var "g", [Var "x"])));
        ("g", Abs (["x"], App (Var "f", [Var "x"])))
      ], App (Var "f", [Constant (Number 42)])));
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
          (Subtype (TySingleton (Number 42)) (TyVar m))
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

  print
    (Context.add (Context.Key.MFA ("m", "f", 0)) (TyFun ([], TySingleton (Atom "ok"))) Context.empty)
    (App (MFA (Constant (Atom "m"), Constant (Atom "f"), Constant (Number 0)), []));
  [%expect {|
    (Ok (
      (TyVar b)
      (Conj (
        (Eq (TyFun () (TySingleton (Atom ok))) (TyFun () (TyVar a)))
        (Subtype
          (TyVar b)
          (TyVar a))
        Empty)))) |}];

  print
    (Context.add (Context.Key.MFA ("m", "f", 0)) (TyFun ([], TySingleton (Atom "ok"))) Context.empty)
    (Let ("M", Constant (Atom "m"),
          Let ("F", Constant (Atom "f"),
               Let ("A", Constant (Number 0),
                    App (MFA (Var "M", Var "F", Var "A"), [])))));
  [%expect {|
    (Ok (
      (TyVar c)
      (Conj (
        Empty (
          Conj (
            Empty (
              Conj (
                Empty (
                  Conj (
                    (Eq (TyVar a) (TyFun () (TyVar b)))
                    (Subtype
                      (TyVar c)
                      (TyVar b))
                    (Conj (
                      Empty Empty Empty
                      (Subtype (TySingleton (Atom   m)) TyAtom)
                      (Subtype (TySingleton (Atom   f)) TyAtom)
                      (Subtype (TySingleton (Number 0)) TyNumber)
                      (Subtype (TyVar a) TyAny))))))))))))) |}]
