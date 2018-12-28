open Base
open Fialyzer
open Ast
open Type
open Derivation
open Constant

let%expect_test "derivation" =
  let print context term =
    Type_variable.reset_count ();
    derive context term
    |> [%sexp_of: (Type.t * constraint_, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  print Context.empty (Constant (Number 42));
  [%expect {| (Ok ((TySingleton (Number 42)) Empty)) |}];

  print Context.empty (Var "x");
  [%expect {| (Error ("Fialyzer.Known_error.FialyzerError(_)")) |}];

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

  (*
   * case 42 of
   *   1 when true -> 1
   * end
   *)
  print Context.empty (Case
    (Constant (Number 42),
      [(PatConstant (Number 1), Constant (Atom "true")), Constant (Number 1)]));
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
            (Eq (TyVar a) (TySingleton (Number 1)))
            (Eq
              (TySingleton (Number 42))
              (TySingleton (Number 1)))
            Empty
            Empty
            Empty))))
        Empty))))
  |}];

  (*
   * case a of
   *   b when true -> c
   * end
   *)
  print Context.empty (Case
    (Constant (Atom "a"),
      [(PatConstant (Atom "b"), Constant (Atom "true")), Constant (Atom "c")]));
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
            (Eq (TyVar a) (TySingleton (Atom c)))
            (Eq
              (TySingleton (Atom a))
              (TySingleton (Atom b)))
            Empty
            Empty
            Empty))))
        Empty))))
  |}];

  print Context.empty (Abs (["X"], Var "X"));
  [%expect {|
    (Ok ((TyFun ((TyVar a)) (TyVar a)) Empty)) |}];

  print Context.empty (Abs (["x"; "y"; "z"], Var "x"));
  [%expect {|
    (Ok (
      (TyFun
        ((TyVar a)
         (TyVar b)
         (TyVar c))
        (TyVar a))
      Empty))
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

  print Context.empty (App (Abs (["X"], Var "X"), [Constant (Number 42)]));
  [%expect {|
    (Ok (
      (TyVar d)
      (Conj (
        (Eq
          (TyFun ((TyVar a)) (TyVar a))
          (TyFun ((TyVar b)) (TyVar c)))
        (Subtype
          (TyVar d)
          (TyVar c))
        Empty
        (Subtype (TySingleton (Number 42)) (TyVar b))
        Empty))))
    |}];

  print Context.empty (App (Abs (["X"; "Y"], Var "X"), [Constant (Number 42); Constant (Number 57)]));
  [%expect {|
    (Ok (
      (TyVar f)
      (Conj (
        (Eq
          (TyFun
            ((TyVar a)
             (TyVar b))
            (TyVar a))
          (TyFun
            ((TyVar c)
             (TyVar d))
            (TyVar e)))
        (Subtype
          (TyVar f)
          (TyVar e))
        Empty
        (Subtype (TySingleton (Number 42)) (TyVar c))
        Empty
        (Subtype (TySingleton (Number 57)) (TyVar d))
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
        ("f", Abs (["X"], App (Var "g", [Var "X"])));
        ("g", Abs (["X"], App (Var "f", [Var "X"])))
      ], App (Var "f", [Constant (Number 42)])));
  [%expect {|
    (Ok (
      (TyVar m)
      (Conj (
        (Conj (
          (Eq (TyVar a) (TyFun ((TyVar k)) (TyVar l)))
          (Subtype
            (TyVar m)
            (TyVar l))
          Empty
          (Subtype (TySingleton (Number 42)) (TyVar k))
          Empty))
        (Eq (TyVar a) (TyFun ((TyVar c)) (TyVar f)))
        (Conj (
          (Eq (TyVar b) (TyFun ((TyVar d)) (TyVar e)))
          (Subtype
            (TyVar f)
            (TyVar e))
          Empty
          (Subtype
            (TyVar c)
            (TyVar d))
          Empty))
        (Eq (TyVar b) (TyFun ((TyVar g)) (TyVar j)))
        (Conj (
          (Eq (TyVar a) (TyFun ((TyVar h)) (TyVar i)))
          (Subtype
            (TyVar j)
            (TyVar i))
          Empty
          (Subtype
            (TyVar g)
            (TyVar h))
          Empty)))))) |}];

  print
    (Context.add (Context.Key.MFA {module_name="m"; function_name="f"; arity=0}) (TyFun ([], TySingleton (Atom "ok"))) Context.empty)
    (App (MFA {module_name=Constant (Atom "m"); function_name=Constant (Atom "f"); arity=Constant (Number 0)}, []));
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
    (Context.add (Context.Key.MFA {module_name="m"; function_name="f"; arity=0}) (TyFun ([], TySingleton (Atom "ok"))) Context.empty)
    (Let ("M", Constant (Atom "m"),
          Let ("F", Constant (Atom "f"),
               Let ("A", Constant (Number 0),
                    App (MFA {module_name=Var "M"; function_name=Var "F"; arity=Var "A"}, [])))));
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
