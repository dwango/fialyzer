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

  print Context.empty (Constant (-1, Number (Int 42)));
  [%expect {| (Ok (42 Empty)) |}];

  print Context.empty (Ref (3, Var "x"));
  [%expect {|
    (Error (
      lib/known_error.ml.FialyzerError (
        UnboundVariable
        (filename TODO:filename)
        (line     3)
        (variable (Var x))))) |}];

  print (Context.add (Context.Key.Var "x") (Type.of_elem TyNumber) Context.empty) (Ref (1, Var "x"));
  [%expect {| (Ok ("number()" Empty)) |}];

  print Context.empty (Tuple (-1, [Constant (-1, Number (Int 42)); Constant (-1, Atom "x")]));
  [%expect {|
    (Ok ("{42, 'x'}" (Conj (Empty Empty))))
  |}];

  (*
   * case 42 of
   *   X when true -> X
   * end
   *)
  print Context.empty (Case (-1, Constant (-1, Number (Int 42)), [(PatVar (-1, "X"), Constant (-1, Atom "true")), Ref (-1, Var "X")]));
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
  print Context.empty (Case (-1, Tuple (-1, [Constant (-1, Number (Int 41)); Constant (-1, Number (Int 42))]), [(PatTuple (-1, [PatVar (-1, "X"); PatVar (-1, "Y")]), Constant (-1, Atom "true")), Tuple (-1, [Ref (-1, Var "X"); Ref (-1, Var "Y")])]));
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
    (-1, Constant (-1, Number (Int 42)),
      [(PatVar (-1, "X"), Constant (-1, Atom "false")), Ref (-1, Var "X");
       (PatVar (-1, "X"), Constant (-1, Atom "true")), Ref (-1, Var "X")]));
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
    (-1, Constant (-1, Number (Int 42)),
      [(PatConstant (-1, Number (Int 1)), Constant (-1, Atom "true")), Constant (-1, Number (Int 1))]));
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
    (-1, Constant (-1, Atom "a"),
      [(PatConstant (-1, Atom "b"), Constant (-1, Atom "true")), Constant (-1, Atom "c")]));
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
    (-1, Constant (2, (Number (Int 123))),
      [(PatVar (-1, "X"), Constant (3, Atom "true")), Ref(4, Var "X")]));
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

  print Context.empty (Abs (-1, {args=["X"]; body=Ref (3, Var "X")}));
  [%expect {|
    (Ok ("fun((a) -> a)" Empty)) |}];

  print Context.empty (Abs (-1, {args=["x"; "y"; "z"]; body=Ref (1, Var "x")}));
  [%expect {|
  (Ok ("fun((a, b, c) -> a)" Empty)) |}];

  print Context.empty (App (3, Constant (1, Number (Int 57)), [Constant (2, Number (Int 42))]));
  [%expect {|
    (Ok (
      c (
        Conj (
          (Eq      57 "fun((a) -> b)")
          (Subtype c  b)
          Empty
          (Subtype 42 a)
          Empty)))) |}];

  print Context.empty (App (3, Constant (-1, Atom "I am a function!"), [Constant (-1, Number (Int 42)); Constant (-1, Number (Int 57))]));
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

  print Context.empty (App (3, Abs (-1, {args=["X"]; body=Ref (-1, Var "X")}), [Constant (-1, Number (Int 42))]));
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

  print Context.empty (App (3, Abs (-1, {args=["X"; "Y"]; body=Ref (-1, Var "X")}), [Constant (-1, Number (Int 42)); Constant (-1, Number (Int 57))]));
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

  print Context.empty (Let (-1, "x", Constant (-1, Number (Int 42)), Ref (-1, Var "x")));
  [%expect {|
    (Ok (42 (Conj (Empty Empty)))) |}];

  let x = LocalFun {function_name="x"; arity=0} in
  print Context.empty (Letrec (-1, [(x, {args=[]; body=Constant (-1, Number (Int 42))})], Ref (-1, x)));
  [%expect {| (Ok (a (Conj (Empty (Eq a "fun(() -> 42)") Empty)))) |}];

  let ref_f = LocalFun {function_name="f"; arity=1} in
  let ref_g = LocalFun {function_name="g"; arity=1} in
  print
    Context.empty
    (Letrec
      (-1, [
        (ref_f, {args=["X"]; body=App (4, Ref (4, ref_g), [Ref (1, Var "X")])});
        (ref_g, {args=["X"]; body=App (5, Ref (5, ref_f), [Ref (2, Var "X")])})
      ], App (6, Ref (6, LocalFun {function_name="f"; arity=1}), [Constant (3, (Number (Int 42)))])));
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
    (App (-1, Ref (-1, MFA {module_name=Constant (-1, Atom "m"); function_name=Constant (-1, Atom "f"); arity=Constant (-1, Number (Int 0))}), []));
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
               Let (-1, "A", Constant (-1, Number (Int 0)),
                    App (-1, Ref (-1, MFA {module_name=Ref (-1, Var "M"); function_name=Ref (-1, Var "F"); arity=Ref (-1, Var "A")}), [])))));
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

  (* [] *)
  print Context.empty (ListNil (-1));
  [%expect {| (Ok ("[none()]" Empty)) |}];

  (* [1|[]] *)
  print Context.empty (ListCons (-1, Constant(-1, Number (Int 1)), ListNil (-1)));
  [%expect {|
    (Ok ("[a | 1]" (Conj ((Eq "[none()]" [a]) Empty Empty)))) |}];

  (* [1|2|[]] *)
  print Context.empty (ListCons (-1, Constant(-1, Number (Int 1)), ListCons(-1, Constant(-1, Number (Int 2)), ListNil (-1))));
  [%expect {|
    (Ok (
      "[a | 1]" (
        Conj ((Eq "[b | 2]" [a]) Empty (Conj ((Eq "[none()]" [b]) Empty Empty)))))) |}];

  (* [] *)
  print Context.empty (ListNil (-1));
  [%expect {| (Ok ("[none()]" Empty)) |}];


  (*
   * case [] of
   *   [] when true -> 1
   * end
   *)
  print Context.empty (Case (-1, ListNil (-1), [(PatNil (-1), Constant (-1, Atom "true")), Constant(-1, Number (Int 1))])); 
  [%expect {|
     (Ok (
       a (
         Conj (
           (Disj ((
             Conj (
               (Subtype 'true'   "'true' | 'false'")
               (Eq      a        1)
               (Eq      "[none()]"
               "[none()]")
               Empty
               Empty
               Empty))))
           Empty))))
  |}];

  (*
   * case [1] of
   *   [1] when true -> 1
   * end
   *)
  print Context.empty (Case (-1, ListCons (-1, Constant (-1, Number (Int 1)), ListNil (-1)), [(PatCons (-1, (PatConstant (-1, Number (Int 1)), PatNil (-1))) , Constant (-1, Atom "true")), Constant(-1, Number (Int 1))])); 
  [%expect {|
     (Ok (
       b (
         Conj (
           (Disj ((
             Conj (
               (Subtype 'true'  "'true' | 'false'")
               (Eq      b       1)
               (Eq      "[a | 1]"
               "[c | 1]")
               (Conj ((Eq "[none()]" [c]) Empty Empty))
               Empty
               Empty))))
           (Conj ((Eq "[none()]" [a]) Empty Empty))))))
  |}];
  

  ()

let%expect_test "pattern_to_expr" =
  let print pat =
    pattern_to_expr pat
    |> [%sexp_of: Ast.t]
    |> Expect_test_helpers_kernel.print_s
  in

  print (PatVar (1, "X"));
  [%expect {| (Ref 1 (Var X)) |}];

  print (PatTuple (1, [PatVar (2, "X"); PatVar (3, "Y"); PatVar (4, "Z")]));
  [%expect {|
    (Tuple 1 (
      (Ref 2 (Var X))
      (Ref 3 (Var Y))
      (Ref 4 (Var Z)))) |}];

  print (PatConstant (1, (Constant.Atom "a")));
  [%expect {| (Constant 1 (Atom a)) |}];

  (* TODO: uncomment below after PatNil is supported *)
  (* print (PatCons (PatVar "X", PatCons (PatVar "Y", PatNil)));
   * [%expect {| (ListCons (Var -1 X) (ListCons (Var -1 Y) ListNil)) |}]; *)

  print (PatMap (1, [(PatConstant (2, Constant.Atom "a"), PatVar (3, "A")); (PatConstant (4, Constant.Atom "b"), PatVar (5, "B"))]));
  [%expect {|
    (MapCreation 1 (
      ((Constant 2 (Atom a))
       (Ref      3 (Var  A)))
      ((Constant 4 (Atom b))
       (Ref      5 (Var  B))))) |}]

let%expect_test "variables_in_pattern" =
  let print pat =
    Type_variable.reset_count ();
    variables_in_pattern pat
    |> List.map ~f:(fun (v, t) -> (v, Type.pp t))
    |> [%sexp_of: (string * string) list]
    |> Expect_test_helpers_kernel.print_s
  in

  print (PatVar (1, "X"));
  [%expect {| ((X a)) |}];

  print (PatTuple (-1, [PatVar (-1, "X"); PatVar (-1, "Y"); PatVar (-1,"Z")]));
  [%expect {|
    ((X a)
     (Y b)
     (Z c)) |}];

  print (PatConstant (-1, (Constant.Atom "a")));
  [%expect {| () |}];

  print (PatCons (-1, ((PatVar (-1, "X"), PatCons (-1, (PatVar (-1, "Y"), PatNil (-1)))))));
  [%expect {|
    ((X b)
     (Y a)) |}];

  print (PatMap (1, [(PatConstant (-1, Constant.Atom "a"), PatVar (-1, "A")); (PatConstant (-1, Constant.Atom "b"), PatVar (-1, "B"))]));
  [%expect {|
    ((A a)
     (B b)) |}]
