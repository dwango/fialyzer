open Base
open Fialyzer
open From_erlang
open Obeam.Abstract_format

let%expect_test "code_to_module" =
  let print code =
    Variable.reset_count();
    code_to_module code
    |> Result.ok_exn
    |> [%sexp_of: Ast.module_]
    |> Expect_test_helpers_kernel.print_s
  in

  let line = 111 in

  (* simple module *)
  print (AbstractCode(ModDecl [
                          AttrFile(line, "test.erl", line);
                          AttrMod(line, "test");
                          AttrExport(line, [("f", 1)]);
                          DeclFun(line, "f", 0, [
                                      ClsFun(line, [PatVar(line, "X")], None, ExprBody[ExprVar(line, "X")])
                                 ]);
                          FormEof
        ]));
  [%expect {|
     ((file test.erl)
      (name test)
      (export ())
      (functions (((specs ()) (fun_name f) (args (X)) (body (Var X))))))
  |}];

  (* patterns in toplevel *)
  print (AbstractCode(ModDecl [
                          AttrFile(line, "patterns.erl", line);
                          AttrMod(line, "patterns");
                          AttrExport(line, [("f", 1)]);
                          DeclFun(line, "f", 0, [
                                      ClsFun(line, [PatLit(LitAtom(line, "a"))], None, ExprBody[ExprLit(LitInteger(line, 10))]);
                                      ClsFun(line, [PatLit(LitAtom(line, "b"))], None, ExprBody[ExprLit(LitInteger(line, 20))]);
                                 ]);
                          FormEof;
        ]));
  [%expect {|
     ((file patterns.erl)
      (name patterns)
      (export ())
      (functions ((
        (specs ())
        (fun_name f)
        (args (__A__))
        (body (
          Case
          (Tuple ((Var __A__)))
          ((((PatTuple ((PatConstant (Atom a)))) (Constant (Atom true)))
            (Constant (Number 10)))
           (((PatTuple ((PatConstant (Atom b)))) (Constant (Atom true)))
            (Constant (Number 20))))))))))
  |}]

let%expect_test "from_erlang" =
  let print abstract_format =
    Variable.reset_count ();
    expr_of_erlang_expr abstract_format
    |> [%sexp_of: Ast.t]
    |> Expect_test_helpers_kernel.print_s in


  (* X *)
  print (ExprVar(1, "X"));
  [%expect {|
    (Var X)
  |}];

  (* {X, Y, Z} *)
  print (ExprTuple(1, [ExprVar(1, "X"); ExprVar(1, "Y"); ExprVar(1, "Z")]));
  [%expect {|
    (Tuple (
      (Var X)
      (Var Y)
      (Var Z)))
  |}];


  (*
   * fun (X) -> X end
   *)
  print (ExprFun(1, None, [
    ClsFun(1, [PatVar(1, "X")], None, ExprVar(1, "X"))
  ]));
  [%expect {|
    (Abs (X) (Var X))
  |}];

  (*
   * fun F(X) -> F(X) end
   *)
  print (ExprFun(1, Some("F"), [
    ClsFun(1, [PatVar(1, "X")], None, ExprLocalCall(1, ExprVar(1, "F"), [ExprVar(1, "X")]))
  ]));
  [%expect {|
    (Letrec ((F (Abs (X) (App (Var F) ((Var X)))))) (Var F))
  |}];

  (*
   * fun (X, {Y, Z}) -> {X, Y, Z};
   *     (X, Y) -> {X, Y}
   * end
   *)
  print (ExprFun(1, None, [
    ClsFun(1, [PatVar(1, "X"); PatTuple(1, [PatVar(1, "Y"); PatVar(1, "Z")])], None, ExprTuple(1, [ExprVar(1, "X"); ExprVar(1, "Y"); ExprVar(1, "Z")]));
    ClsFun(1, [PatVar(1, "X"); PatVar(1, "Y")], None, ExprTuple(1, [ExprVar(1, "X"); ExprVar(1, "Y")]))
  ]));
  [%expect {|
    (Abs
      (__A__ __B__)
      (Case
        (Tuple (
          (Var __A__)
          (Var __B__)))
        ((((PatTuple (
             (PatVar X)
             (PatTuple (
               (PatVar Y)
               (PatVar Z)))))
           (Constant (Atom true)))
          (Tuple (
            (Var X)
            (Var Y)
            (Var Z))))
         (((PatTuple (
             (PatVar X)
             (PatVar Y)))
           (Constant (Atom true)))
          (Tuple (
            (Var X)
            (Var Y)))))))
  |}];

  (*
   * fun (x) -> y  end
   *)
  print (ExprFun(1, None, [
    ClsFun(1, [PatLit (LitAtom(1, "x"))], None, ExprLit(LitAtom(1, "y")))
  ]));
  [%expect {|
    (Abs
      (__A__)
      (Case
        (Tuple ((Var __A__)))
        ((
          ((PatTuple ((PatConstant (Atom x)))) (Constant (Atom true)))
          (Constant (Atom y))))))
  |}];


  (*
   * fun (1) -> 2  end
   *)
  print (ExprFun(1, None, [
    ClsFun(1, [PatLit (LitInteger(1, 42))], None, ExprLit(LitInteger(1, 43)))
  ]));
  [%expect {|
    (Abs
      (__A__)
      (Case
        (Tuple ((Var __A__)))
        ((
          ((PatTuple ((PatConstant (Number 42)))) (Constant (Atom true)))
          (Constant (Number 43))))))
  |}];


  (*
   * fun ([]) -> [];
   *     ([H|T]) -> T
   * end
   *)
  print (ExprFun (1, None, [
    ClsFun (1, [PatNil 1], None, ExprNil 1);
    ClsFun (2, [PatCons (2, PatVar (2, "H"), PatVar (2, "T"))], None, ExprVar (2, "T"))
  ]));
  [%expect {|
    (Abs
      (__A__)
      (Case
        (Tuple ((Var __A__)))
        ((((PatTuple (PatNil)) (Constant (Atom true))) ListNil)
         (((PatTuple ((
             PatCons
             (PatVar H)
             (PatVar T))))
           (Constant (Atom true)))
          (Var T))))) |}];

  (*
   * fun ("abc") -> ok end
   *)
  print (ExprFun (1, None, [
    ClsFun (1, [PatLit (LitString (1, "abc"))], None, ExprLit (LitAtom (1, "ok")))
  ]));
  [%expect {|
    (Abs
      (__A__)
      (Case
        (Tuple ((Var __A__)))
        ((
          ((PatTuple ((
             PatCons
             (PatConstant (Number 97))
             (PatCons
               (PatConstant (Number 98))
               (PatCons (PatConstant (Number 99)) PatNil)))))
           (Constant (Atom true)))
          (Constant (Atom ok)))))) |}];

  (*
   * [1,2,3]
   *)
  print (ExprCons (1, ExprLit (LitInteger (1, 1)),
                   ExprCons (1, ExprLit (LitInteger (1, 2)),
                             ExprCons (1, ExprLit (LitInteger (1, 3)),
                                       ExprNil 1))));
  [%expect {|
    (ListCons
      (Constant (Number 1))
      (ListCons (Constant (Number 2)) (ListCons (Constant (Number 3)) ListNil))) |}];

  (*
   * "abc"
   *)
  print (ExprLit (LitString (1, "abc")));
  [%expect {|
    (ListCons
      (Constant (Number 97))
      (ListCons (Constant (Number 98)) (ListCons (Constant (Number 99)) ListNil))) |}];

  (*
   * A = B = 1
   *)
  print (ExprMatch (1, PatVar (1, "A"), ExprMatch (1, PatVar (1, "B"), ExprLit (LitInteger (1, 1)))));
  [%expect {|
    (Case
      (Constant (Number 1))
      ((
        ((PatVar B) (Constant (Atom true)))
        (Case
          (Constant (Number 1))
          ((((PatVar A) (Constant (Atom true))) (Constant (Number 1)))))))) |}];

  (*
   * fun () -> A = 1, B = 2, A + B end
   *)
  print (ExprFun (1, None,
                  [ClsFun (1, [], None,
                           ExprBody [ExprMatch (1, PatVar (1, "A"), ExprLit (LitInteger (1, 1)));
                                     ExprMatch (1, PatVar (1, "B"), ExprLit (LitInteger (1, 2)));
                                     ExprBinOp (1, "+", ExprVar (1, "A"), ExprVar (1, "B"))])]));
  [%expect {|
    (Abs
      ()
      (Case
        (Constant (Number 1))
        ((
          ((PatVar A) (Constant (Atom true)))
          (Case
            (Constant (Number 2))
            ((
              ((PatVar B) (Constant (Atom true)))
              (App
                (MFA
                  (module_name   (Constant (Atom   erlang)))
                  (function_name (Constant (Atom   +)))
                  (arity         (Constant (Number 2))))
                ((Var A)
                 (Var B)))))))))) |}]

let%expect_test "extract_match_expr" =
  let print expr =
    extract_match_expr expr
    |> [%sexp_of: expr_t list]
    |> Expect_test_helpers_kernel.print_s
  in

  (* input: A = B *)
  (* output: A = B *)
  print (ExprMatch (1, PatVar (2, "A"), ExprVar (3, "B")));
  [%expect {|
    ((
      ExprMatch 1
      (PatVar  2 A)
      (ExprVar 3 B))) |}];

  (* input: A = B = C *)
  (* output: B = C, A = B *)
  print (ExprMatch (1, PatVar (2, "A"), ExprMatch (3, PatVar (4, "B"), ExprVar (5, "C"))));
  [%expect {|
    ((ExprMatch 3
       (PatVar  4 B)
       (ExprVar 5 C))
     (ExprMatch 1
       (PatVar  2 A)
       (ExprVar 5 C))) |}];

  (* input: ExprBody [A = B = C, A] *)
  (* output: ExprBody [B = C, A = C, A] *)
  print (ExprBody [ExprMatch (1, PatVar (2, "A"), ExprMatch (3, PatVar (4, "B"), ExprVar (5, "C")));
                   ExprVar (6, "A")]);
  [%expect {|
    ((
      ExprBody (
        (ExprMatch 3
          (PatVar  4 B)
          (ExprVar 5 C))
        (ExprMatch 1
          (PatVar  2 A)
          (ExprVar 5 C))
        (ExprVar 6 A)))) |}];

  (* input: case A = B of C -> D = E = F; G -> H = I = J end *)
  (* output: A = B, case B of C -> E = F, D = F; G -> I = J, H = J end *)
  print (ExprCase (1, ExprMatch (2, PatVar (3, "A"), ExprVar (4, "B")),
                   [ClsCase (5, PatVar (6, "C"), None,
                             ExprMatch (7, PatVar (8, "D"), ExprMatch (9, PatVar (10, "E"), ExprVar (11, "F"))));
                    ClsCase (5, PatVar (12, "G"), None,
                             ExprMatch (13, PatVar (14, "H"), ExprMatch (15, PatVar (16, "I"), ExprVar (17, "J"))))]));
  [%expect {|
    ((ExprMatch 2
       (PatVar  3 A)
       (ExprVar 4 B))
     (ExprCase 1
       (ExprVar 4 B)
       ((ClsCase 5
          (PatVar 6 C)
          ()
          (ExprBody (
            (ExprMatch 9
              (PatVar  10 E)
              (ExprVar 11 F))
            (ExprMatch 7
              (PatVar  8  D)
              (ExprVar 11 F)))))
        (ClsCase 5
          (PatVar 12 G)
          ()
          (ExprBody (
            (ExprMatch 15
              (PatVar  16 I)
              (ExprVar 17 J))
            (ExprMatch 13
              (PatVar  14 H)
              (ExprVar 17 J)))))))) |}];

  (* input: [ N = 1 | M = 2 ] *)
  (* output: N = 1, M = 2, [ 1 | 2 ] *)
  print (ExprCons (1,
                   ExprMatch (2, PatVar (3, "N"), ExprLit (LitInteger (4, 1))),
                   ExprMatch (5, PatVar (6, "M"), ExprLit (LitInteger (7, 2)))));
  [%expect {|
    ((ExprMatch 2 (PatVar 3 N) (ExprLit (LitInteger 4 1)))
     (ExprMatch 5 (PatVar 6 M) (ExprLit (LitInteger 7 2)))
     (ExprCons 1
       (ExprLit (LitInteger 4 1))
       (ExprLit (LitInteger 7 2)))) |}];

  (* input: [] *)
  (* output: [] *)
  print (ExprNil 1);
  [%expect {| ((ExprNil 1)) |}];

  (* TODO *)
  (* input: [ A = B || B <- [C = D = 1], (E = 2) =:= (F = 3) ] *)
  (* output: ? *)

  (* input: fun f/0 *)
  (* output: fun f/0 *)
  print (ExprLocalFunRef (1, "f", 0));
  [%expect {| ((ExprLocalFunRef 1 f 0)) |}];

  (* input: fun m:f/0 *)
  (* output: fun m:f/0 *)
  print (ExprRemoteFunRef (1, AtomVarAtom (2, "m"), AtomVarAtom (3, "f"), IntegerVarInteger (4, 0)));
  [%expect {|
    ((
      ExprRemoteFunRef 1
      (AtomVarAtom       2 m)
      (AtomVarAtom       3 f)
      (IntegerVarInteger 4 0))) |}];

  (* input: fun () -> A = B = C; () -> D = E = F end *)
  (* output: fun () -> B = C, A = C; () -> E = F, D = F end *)
  print (ExprFun (1, None,
                  [ClsFun (2, [], None, ExprMatch (3, PatVar (4, "A"), ExprMatch (5, PatVar (6, "B"), ExprVar (7, "C"))));
                   ClsFun (8, [], None, ExprMatch (9, PatVar (10, "D"), ExprMatch (11, PatVar (12, "E"), ExprVar (13, "F"))))]));
  [%expect {|
    ((
      ExprFun 1
      ()
      ((ClsFun 2
         ()
         ()
         (ExprBody (
           (ExprMatch 5
             (PatVar  6 B)
             (ExprVar 7 C))
           (ExprMatch 3
             (PatVar  4 A)
             (ExprVar 7 C)))))
       (ClsFun 8
         ()
         ()
         (ExprBody (
           (ExprMatch 11
             (PatVar  12 E)
             (ExprVar 13 F))
           (ExprMatch 9
             (PatVar  10 D)
             (ExprVar 13 F)))))))) |}];

  (* input: f(N = 1, M = 2) *)
  (* output: M = 2, N = 1, f(1, 2) *)
  print (ExprLocalCall (1, ExprVar (2, "f"), [ExprMatch (3, PatVar (4, "N"), ExprLit (LitInteger (6, 1)));
                                              ExprMatch (7, PatVar (8, "M"), ExprLit (LitInteger (10, 2)))]));
  [%expect {|
    ((ExprMatch 7 (PatVar 8 M) (ExprLit (LitInteger 10 2)))
     (ExprMatch 3 (PatVar 4 N) (ExprLit (LitInteger 6 1)))
     (ExprLocalCall 1
       (ExprVar 2 f)
       ((ExprLit (LitInteger 6  1))
        (ExprLit (LitInteger 10 2))))) |}];

  (* input: (M = m):(F = f)(A = 1, B = 2) *)
  (* output: M = m, F = f, B = 2, A = 1, m:f(1, 2) *)
  print (ExprRemoteCall (1, 2,
                         ExprMatch (3, PatVar (4, "M"), ExprLit (LitAtom (5, "m"))),
                         ExprMatch (6, PatVar (7, "F"), ExprLit (LitAtom (8, "f"))),
                         [ExprMatch (9, PatVar (10, "A"), ExprLit (LitInteger (11, 1)));
                          ExprMatch (12, PatVar (13, "B"), ExprLit (LitInteger (14, 2)))]));
  [%expect {|
    ((ExprMatch 3 (PatVar 4 M) (ExprLit (LitAtom 5 m)))
     (ExprMatch 6 (PatVar 7 F) (ExprLit (LitAtom 8 f)))
     (ExprMatch 12 (PatVar 13 B) (ExprLit (LitInteger 14 2)))
     (ExprMatch 9 (PatVar 10 A) (ExprLit (LitInteger 11 1)))
     (ExprRemoteCall 1 2
       (ExprLit (LitAtom 5 m))
       (ExprLit (LitAtom 8 f))
       ((ExprLit (LitInteger 11 1))
        (ExprLit (LitInteger 14 2))))) |}];

  (* input: #{K1 = k1 => V1 = v1, K2 = k2 := V2 = v2} *)
  (* output: K2 = k2, V2 = v2, K1 = k1, V1 = v1, #{k1 => v1, k2 := v2} *)
  print (ExprMapCreation (1,
                          [ExprAssoc (2,
                                      ExprMatch (3, PatVar (4, "K1"), ExprLit (LitAtom (5, "k1"))),
                                      ExprMatch (6, PatVar (7, "V1"), ExprLit (LitAtom (8, "v1"))));
                           ExprAssocExact (9,
                                           ExprMatch (10, PatVar (11, "K2"), ExprLit (LitAtom (12, "k2"))),
                                           ExprMatch (15, PatVar (14, "V2"), ExprLit (LitAtom (13, "v2"))))]));
  [%expect {|
    ((ExprMatch 10 (PatVar 11 K2) (ExprLit (LitAtom 12 k2)))
     (ExprMatch 15 (PatVar 14 V2) (ExprLit (LitAtom 13 v2)))
     (ExprMatch 3 (PatVar 4 K1) (ExprLit (LitAtom 5 k1)))
     (ExprMatch 6 (PatVar 7 V1) (ExprLit (LitAtom 8 v1)))
     (ExprMapCreation 1 (
       (ExprAssoc 2
         (ExprLit (LitAtom 5 k1))
         (ExprLit (LitAtom 8 v1)))
       (ExprAssocExact 9
         (ExprLit (LitAtom 12 k2))
         (ExprLit (LitAtom 13 v2)))))) |}];

  (* input: (M = #{})#{K1 = k1 => V1 = v1, K2 = k2 => V2 = v2} *)
  (* output: K2 = k2, V2 = v2, K1 = k1, V1 = v1, #{k1 => v1, k2 => v2} *)
  print (ExprMapCreation (1,
                          [ExprAssoc (2,
                                      ExprMatch (3, PatVar (4, "K1"), ExprLit (LitAtom (5, "k1"))),
                                      ExprMatch (6, PatVar (7, "V1"), ExprLit (LitAtom (8, "v1"))));
                           ExprAssoc (9,
                                      ExprMatch (10, PatVar (11, "K2"), ExprLit (LitAtom (12, "k2"))),
                                      ExprMatch (15, PatVar (14, "V2"), ExprLit (LitAtom (13, "v2"))))]));
  [%expect {|
    ((ExprMatch 10 (PatVar 11 K2) (ExprLit (LitAtom 12 k2)))
     (ExprMatch 15 (PatVar 14 V2) (ExprLit (LitAtom 13 v2)))
     (ExprMatch 3 (PatVar 4 K1) (ExprLit (LitAtom 5 k1)))
     (ExprMatch 6 (PatVar 7 V1) (ExprLit (LitAtom 8 v1)))
     (ExprMapCreation 1 (
       (ExprAssoc 2
         (ExprLit (LitAtom 5 k1))
         (ExprLit (LitAtom 8 v1)))
       (ExprAssoc 9
         (ExprLit (LitAtom 12 k2))
         (ExprLit (LitAtom 13 v2)))))) |}];

  (* input: (M = #{})#{K1 = k1 => V1 = v1, K2 = k2 := V2 = v2} *)
  (* output: M = #{}, K2 = k2, V2 = v2, K1 = k2, V1 = v2, (#{})#{k1 => v1, k2 => v2} *)
  print (ExprMapUpdate (1,
                        ExprMatch (2, PatVar (3, "M"), ExprMapCreation (4, [])),
                        [ExprAssoc (5,
                                    ExprMatch (6, PatVar (7, "K1"), ExprLit (LitAtom (8, "k1"))),
                                    ExprMatch (9, PatVar (10, "V1"), ExprLit (LitAtom (11, "v1"))));
                         ExprAssocExact (12,
                                         ExprMatch (13, PatVar (14, "K2"), ExprLit (LitAtom (15, "k2"))),
                                         ExprMatch (16, PatVar (17, "V2"), ExprLit (LitAtom (18, "v2"))))]));
  [%expect {|
    ((ExprMatch 2 (PatVar 3 M) (ExprMapCreation 4 ()))
     (ExprMatch 13 (PatVar 14 K2) (ExprLit (LitAtom 15 k2)))
     (ExprMatch 16 (PatVar 17 V2) (ExprLit (LitAtom 18 v2)))
     (ExprMatch 6 (PatVar 7 K1) (ExprLit (LitAtom 8 k1)))
     (ExprMatch 9 (PatVar 10 V1) (ExprLit (LitAtom 11 v1)))
     (ExprMapUpdate 1
       (ExprMapCreation 4 ())
       ((ExprAssoc 5
          (ExprLit (LitAtom 8  k1))
          (ExprLit (LitAtom 11 v1)))
        (ExprAssocExact 12
          (ExprLit (LitAtom 15 k2))
          (ExprLit (LitAtom 18 v2)))))) |}];

  (* input: (N = 1) + (M = 2) *)
  (* output: N = 1, M = 2, 1 + 2 *)
  print (ExprBinOp (1, "+",
                    ExprMatch (2, PatVar (3, "N"), ExprLit (LitInteger (4, 1))),
                    ExprMatch (5, PatVar (6, "M"), ExprLit (LitInteger (7, 2)))));
  [%expect {|
    ((ExprMatch 2 (PatVar 3 N) (ExprLit (LitInteger 4 1)))
     (ExprMatch 5 (PatVar 6 M) (ExprLit (LitInteger 7 2)))
     (ExprBinOp 1 +
       (ExprLit (LitInteger 4 1))
       (ExprLit (LitInteger 7 2)))) |}];

  (* input: {N = 1, M = 2} *)
  (* output: M = 2, N = 1, {1, 2} *)
  print (ExprTuple (1,
                    [ExprMatch (2, PatVar (3, "N"), ExprLit (LitInteger (4, 1)));
                     ExprMatch (5, PatVar (6, "M"), ExprLit (LitInteger (7, 2)))]));
  [%expect {|
    ((ExprMatch 5 (PatVar 6 M) (ExprLit (LitInteger 7 2)))
     (ExprMatch 2 (PatVar 3 N) (ExprLit (LitInteger 4 1)))
     (ExprTuple 1 (
       (ExprLit (LitInteger 4 1))
       (ExprLit (LitInteger 7 2))))) |}];

  (* input: A *)
  (* output: A *)
  print (ExprVar (1, "A"));
  [%expect {| ((ExprVar 1 A)) |}];

  (* input: a *)
  (* output: a *)
  print (ExprLit (LitAtom (1, "a")));
  [%expect {| ((ExprLit (LitAtom 1 a))) |}];
