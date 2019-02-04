open Base
open Fialyzer
open Ast
open Type
open Derivation
open Constant
module C = Constraint

let rec sexp_of_constraint = function
  | C.Eq {lhs=s; rhs=t; _} ->
     [%sexp_of: string * string * string] ("Eq", Type.pp s, Type.pp t)
  | C.Subtype {lhs=s; rhs=t; _} ->
     [%sexp_of: string * string * string] ("Subtype", Type.pp s, Type.pp t)
  | C.Conj ts ->
     [%sexp_of: string * Sexp.t list] ("Conj", List.map ~f:sexp_of_constraint ts)
  | C.Disj ts ->
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

  print Context.empty (Constant (-1, Number 42));
  [%expect {| (Ok (42 Empty)) |}];

  print Context.empty (Var (3, "x"));
  [%expect {|
    (Error (
      lib/known_error.ml.FialyzerError (
        UnboundVariable
        (filename TODO:filename)
        (line     3)
        (variable (Var x))))) |}];

  print (Context.add (Context.Key.Var "x") (Type.of_elem TyNumber) Context.empty) (Var (1, "x"));
  [%expect {| (Ok ("number()" Empty)) |}];

  print Context.empty (Tuple (-1, [Constant (-1, Number 42); Constant (-1, Atom "x")]));
  [%expect {|
    (Ok ("{42, 'x'}" (Conj (Empty Empty))))
  |}];

  (*
   * case 42 of
   *   X when true -> X
   * end
   *)
  print Context.empty (Case (Constant (-1, Number 42), [(PatVar "X", Constant (-1, Atom "true")), Var (-1, "X")]));
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
  print Context.empty (Case (Tuple (-1, [Constant (-1, Number 41); Constant (-1, Number 42)]), [(PatTuple [PatVar "X"; PatVar "Y"], Constant (-1, Atom "true")), Tuple (-1, [Var (-1, "X"); Var (-1, "Y")])]));
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
    (Constant (-1, Number 42),
      [(PatVar "X", Constant (-1, Atom "false")), Var (-1, "X");
       (PatVar "X", Constant (-1, Atom "true")), Var (-1, "X")]));
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
    (Constant (-1, Number 42),
      [(PatConstant (Number 1), Constant (-1, Atom "true")), Constant (-1, Number 1)]));
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
    (Constant (-1, Atom "a"),
      [(PatConstant (Atom "b"), Constant (-1, Atom "true")), Constant (-1, Atom "c")]));
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
    (Constant (2, (Number 123)),
      [(PatVar "X", Constant (3, Atom "true")), Var(4, "X")]));
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

  print Context.empty (Abs (-1, {args=["X"]; body=Var (3, "X")}));
  [%expect {|
    (Ok ("fun((a) -> a)" Empty)) |}];

  print Context.empty (Abs (-1, {args=["x"; "y"; "z"]; body=Var (1,"x")}));
  [%expect {|
  (Ok ("fun((a, b, c) -> a)" Empty)) |}];

  print Context.empty (App (3, Constant (1, Number 57), [Constant (2, Number 42)]));
  [%expect {|
    (Ok (
      c (
        Conj (
          (Eq      57 "fun((a) -> b)")
          (Subtype c  b)
          Empty
          (Subtype 42 a)
          Empty)))) |}];

  print Context.empty (App (3, Constant (-1, Atom "I am a function!"), [Constant (-1, Number 42); Constant (-1, Number 57)]));
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

  print Context.empty (App (3, Abs (-1, {args=["X"]; body=Var (-1, "X")}), [Constant (-1, Number 42)]));
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

  print Context.empty (App (3, Abs (-1, {args=["X"; "Y"]; body=Var (-1, "X")}), [Constant (-1, Number 42); Constant (-1, Number 57)]));
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

  print Context.empty (Let (-1, "x", Constant (-1, Number 42), Var (-1, "x")));
  [%expect {|
    (Ok (42 (Conj (Empty Empty)))) |}];

  print Context.empty (Letrec (-1, [("x", {args=[]; body=Constant (-1, Number 42)})], LocalFun {function_name="x"; arity=0}));
  [%expect {| (Ok (a (Conj (Empty (Eq a "fun(() -> 42)") Empty)))) |}];

  print
    Context.empty
    (Letrec
      (-1, [
        ("f", {args=["X"]; body=App (4, LocalFun {function_name="g"; arity=1}, [Var (1, "X")])});
        ("g", {args=["X"]; body=App (5, LocalFun {function_name="f"; arity=1}, [Var (2, "X")])})
      ], App (6, LocalFun {function_name="f"; arity=1}, [Constant (3, (Number 42))])));
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
    (App (-1, MFA {module_name=Constant (-1, Atom "m"); function_name=Constant (-1, Atom "f"); arity=Constant (-1, Number 0)}, []));
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
    (Let (-1, "M", Constant (-1, Atom "m"),
          Let (-1, "F", Constant (-1, Atom "f"),
               Let (-1, "A", Constant (-1, Number 0),
                    App (-1, MFA {module_name=Var (-1, "M"); function_name=Var (-1, "F"); arity=Var (-1, "A")}, [])))));
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
                        (Subtype 0   "number()")))))))))))))
  |}];

  ()
