open Base
open Fialyzer
open Ast
open Type
open Derivation
open Constant

let rec sexp_of_constraint = function
  | Eq (s, t) ->
     [%sexp_of: string * string * string] ("Eq", Type.pp s, Type.pp t)
  | Subtype (s, t) ->
     [%sexp_of: string * string * string] ("Subtype", Type.pp s, Type.pp t)
  | Conj ts ->
     [%sexp_of: string * Sexp.t list] ("Conj", List.map ~f:sexp_of_constraint ts)
  | Disj ts ->
     [%sexp_of: string * Sexp.t list] ("Disj", List.map ~f:sexp_of_constraint ts)
  | Empty ->
     Sexp.Atom "Empty"

let%expect_test "derivation" =
  let print context term =
    Type_variable.reset_count ();
    derive context term
    |> Result.map ~f:(fun (ty, c) -> (Type.pp ty, sexp_of_constraint c))
    |> [%sexp_of: (string * Sexp.t, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s in

  print Context.empty (Constant (Number 42));
  [%expect {| (Ok (42 Empty)) |}];

  print Context.empty (Var "x");
  [%expect {| (Error ("Fialyzer.Known_error.FialyzerError(_)")) |}];

  print (Context.add (Context.Key.Var "x") (Type.of_elem TyNumber) Context.empty) (Var "x");
  [%expect {| (Ok ("number()" Empty)) |}];

  print Context.empty (Tuple [Constant (Number 42); Constant (Atom "x")]);
  [%expect {|
    (Ok ("{42, 'x'}" (Conj (Empty Empty))))
  |}];

  (*
   * case 42 of
   *   X when true -> X
   * end
   *)
  print Context.empty (Case (Constant (Number 42), [(PatVar "X", Constant (Atom "true")), Var "X"]));
  [%expect {|
     (Ok (
       a (
         Conj (
           (Disj ((
             Conj (
               (Subtype 'true' "'true' | 'false'")
               (Eq      a      b)
               (Eq      42     b)
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
       a (
         Conj (
           (Disj ((
             Conj (
               (Subtype 'true'   "'true' | 'false'")
               (Eq      a        "{b, c}")
               (Eq      "{41, 42}"
               "{b, c}")
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
       a (
         Conj (
           (Disj (
             (Conj (
               (Subtype 'false' "'true' | 'false'")
               (Eq      a       b)
               (Eq      42      b)
               Empty
               Empty
               Empty))
             (Conj (
               (Subtype 'true' "'true' | 'false'")
               (Eq      a      c)
               (Eq      42     c)
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
      a (
        Conj (
          (Disj ((
            Conj (
              (Subtype 'true' "'true' | 'false'")
              (Eq      a      1)
              (Eq      42     1)
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
      a (
        Conj (
          (Disj ((
            Conj (
              (Subtype 'true' "'true' | 'false'")
              (Eq      a      'c')
              (Eq      'a'    'b')
              Empty
              Empty
              Empty))))
          Empty))))
  |}];

  (*
   * [X : number()] |-
   * case 123 of
   *   X when X
   * end
   *)
  let ctx = Context.empty |> Context.add (Context.Key.Var "X") (Type.of_elem TyNumber) in
  print ctx (Case
    (Constant (Number 123),
      [(PatVar "X", Constant (Atom "true")), Var "X"]));
  [%expect {|
    (Ok (
      a (
        Conj (
          (Disj ((
            Conj (
              (Subtype 'true' "'true' | 'false'")
              (Eq      a      "number()")
              (Eq      123    "number()")
              Empty
              Empty
              Empty))))
          Empty))))
  |}];

  print Context.empty (Abs (["X"], Var "X"));
  [%expect {|
    (Ok ("fun((a) -> a)" Empty)) |}];

  print Context.empty (Abs (["x"; "y"; "z"], Var "x"));
  [%expect {|
    (Ok ("fun((a, b, c) -> a)" Empty))
   |}];

  print Context.empty (App (Constant (Number 57), [Constant (Number 42)]));
  [%expect {|
    (Ok (
      c (
        Conj (
          (Eq      57 "fun((a) -> b)")
          (Subtype c  b)
          Empty
          (Subtype 42 a)
          Empty)))) |}];

  print Context.empty (App (Constant (Atom "I am a function!"), [Constant (Number 42); Constant (Number 57)]));
  [%expect {|
    (Ok (
      d (
        Conj (
          (Eq      "'I am a function!'" "fun((a, b) -> c)")
          (Subtype d                    c)
          Empty
          (Subtype 42 a)
          Empty
          (Subtype 57 b)
          Empty)))) |}];

  print Context.empty (App (Abs (["X"], Var "X"), [Constant (Number 42)]));
  [%expect {|
    (Ok (
      d (
        Conj (
          (Eq      "fun((a) -> a)" "fun((b) -> c)")
          (Subtype d               c)
          Empty
          (Subtype 42 b)
          Empty))))
    |}];

  print Context.empty (App (Abs (["X"; "Y"], Var "X"), [Constant (Number 42); Constant (Number 57)]));
  [%expect {|
    (Ok (
      f (
        Conj (
          (Eq      "fun((a, b) -> a)" "fun((c, d) -> e)")
          (Subtype f                  e)
          Empty
          (Subtype 42 c)
          Empty
          (Subtype 57 d)
          Empty)))) |}];

  print Context.empty (Let ("x", Constant (Number 42), Var "x"));
  [%expect {|
    (Ok (42 (Conj (Empty Empty)))) |}];

  print Context.empty (Letrec ([("x", Constant (Number 42))], Var "x"));
  [%expect {| (Ok (a (Conj (Empty (Eq a 42) Empty)))) |}];

  print
    Context.empty
    (Letrec
      ([
        ("f", Abs (["X"], App (Var "g", [Var "X"])));
        ("g", Abs (["X"], App (Var "f", [Var "X"])))
      ], App (Var "f", [Constant (Number 42)])));
  [%expect {|
    (Ok (
      m (
        Conj (
          (Conj (
            (Eq      a "fun((k) -> l)")
            (Subtype m l)
            Empty
            (Subtype 42 k)
            Empty))
          (Eq a "fun((c) -> f)")
          (Conj (
            (Eq      b "fun((d) -> e)")
            (Subtype f e)
            Empty
            (Subtype c d)
            Empty))
          (Eq b "fun((g) -> j)")
          (Conj (
            (Eq      a "fun((h) -> i)")
            (Subtype j i)
            Empty
            (Subtype g h)
            Empty)))))) |}];

  print
    (Context.add (Context.Key.MFA {module_name="m"; function_name="f"; arity=0})
                 (Type.of_elem (TyFun ([], Type.of_elem (TySingleton (Atom "ok")))))
                 Context.empty)
    (App (MFA {module_name=Constant (Atom "m"); function_name=Constant (Atom "f"); arity=Constant (Number 0)}, []));
  [%expect {|
    (Ok (
      b (
        Conj (
          (Eq      "fun(() -> 'ok')" "fun(() -> a)")
          (Subtype b                 a)
          Empty)))) |}];

  print
    (Context.add (Context.Key.MFA {module_name="m"; function_name="f"; arity=0})
                 (Type.of_elem (TyFun ([], Type.of_elem (TySingleton (Atom "ok")))))
                 Context.empty)
    (Let ("M", Constant (Atom "m"),
          Let ("F", Constant (Atom "f"),
               Let ("A", Constant (Number 0),
                    App (MFA {module_name=Var "M"; function_name=Var "F"; arity=Var "A"}, [])))));
  [%expect {|
    (Ok (
      c (
        Conj (
          Empty (
            Conj (
              Empty (
                Conj (
                  Empty (
                    Conj (
                      (Eq      a "fun(() -> b)")
                      (Subtype c b)
                      (Conj (
                        Empty Empty Empty
                        (Subtype 'm' "atom()")
                        (Subtype 'f' "atom()")
                        (Subtype 0   "number()")
                        (Subtype a   "any()"))))))))))))) |}]
