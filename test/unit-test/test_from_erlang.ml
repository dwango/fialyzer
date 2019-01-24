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
                          AttrFile {line; file="test.erl"; file_line=line};
                          AttrMod {line; module_name="test"};
                          AttrExport {line; function_arity_list=[("f", 1)]};
                          DeclFun {line; function_name="f"; arity=0; clauses=[
                                       ClsFun {line; patterns=[PatVar {line=line; id="X"}]; guard_sequence=None;
                                               body=ExprBody {exprs=[ExprVar {line; id="X"}]}}
                                  ]};
                          FormEof
        ]));
  [%expect {|
     ((file test.erl)
      (name test)
      (export ())
      (functions (((specs ()) (fun_name f) (fun_abst ((args (X)) (body (Var X))))))))
  |}];

  (* patterns in toplevel *)
  print (AbstractCode(ModDecl [
                          AttrFile {line; file="patterns.erl"; file_line=line};
                          AttrMod {line; module_name="patterns"};
                          AttrExport {line; function_arity_list=[("f", 1)]};
                          DeclFun {line; function_name="f"; arity=0; clauses=[
                                       ClsFun {line; patterns=[PatLit {lit=LitAtom {line; atom="a"}}]; guard_sequence=None;
                                               body=ExprBody {exprs=[ExprLit {lit=LitInteger {line; integer=10}}]}};
                                       ClsFun {line; patterns=[PatLit {lit=LitAtom {line; atom="b"}}]; guard_sequence=None;
                                               body=ExprBody {exprs=[ExprLit {lit=LitInteger {line; integer=20}}]}};
                                  ]};
                          FormEof;
        ]));
  [%expect {|
     ((file patterns.erl)
      (name patterns)
      (export ())
      (functions ((
        (specs ())
        (fun_name f)
        (fun_abst (
          (args (__A__))
          (body (
            Case
            (Tuple ((Var __A__)))
            ((((PatTuple ((PatConstant (Atom a)))) (Constant (Atom true)))
              (Constant (Number 10)))
             (((PatTuple ((PatConstant (Atom b)))) (Constant (Atom true)))
              (Constant (Number 20))))))))))))
  |}]

let%expect_test "from_erlang" =
  let print abstract_format =
    Variable.reset_count ();
    expr_of_erlang_expr abstract_format
    |> [%sexp_of: Ast.t]
    |> Expect_test_helpers_kernel.print_s in


  (* X *)
  print (ExprVar {line=1; id="X"});
  [%expect {|
    (Var X)
  |}];

  (* {X, Y, Z} *)
  print (ExprTuple {line=1; elements=[ExprVar {line=1; id="X"}; ExprVar {line=1; id="Y"}; ExprVar {line=1; id="Z"}]});
  [%expect {|
    (Tuple (
      (Var X)
      (Var Y)
      (Var Z)))
  |}];


  (*
   * fun (X) -> X end
   *)
  print (ExprFun {line=1; name=None; clauses=[
    ClsFun {line=1; patterns=[PatVar {line=1; id="X"}]; guard_sequence=None; body=ExprVar {line=1; id="X"}}
  ]});
  [%expect {|
    (Abs ((args (X)) (body (Var X))))
  |}];

  (*
   * fun F(X) -> F(X) end
   *)
  print (ExprFun {line=1; name=Some("F"); clauses=[
    ClsFun {line=1;
            patterns=[PatVar {line=1; id="X"}];
            guard_sequence=None;
            body=ExprLocalCall {line=1; function_expr=ExprVar {line=1; id="F"}; args=[ExprVar {line=1; id="X"}]}}
  ]});
  [%expect {|
    (Letrec ((F ((args (X)) (body (App (Var F) ((Var X))))))) (Var F))
  |}];

  (*
   * fun (X, {Y, Z}) -> {X, Y, Z};
   *     (X, Y) -> {X, Y}
   * end
   *)
  print (ExprFun {line=1; name=None; clauses=[
    ClsFun {line=1;
            patterns=[PatVar {line=1; id="X"}; PatTuple {line=1; pats=[PatVar {line=1; id="Y"}; PatVar {line=1; id="Z"}]}];
            guard_sequence=None;
            body=ExprTuple {line=1; elements=[ExprVar {line=1; id="X"}; ExprVar {line=1; id="Y"}; ExprVar {line=1; id="Z"}]}};
    ClsFun {line=1;
            patterns=[PatVar {line=1; id="X"}; PatVar {line=1; id="Y"}];
            guard_sequence=None;
            body=ExprTuple {line=1; elements=[ExprVar {line=1; id="X"}; ExprVar {line=1; id="Y"}]}}
  ]});
  [%expect {|
    (Abs (
      (args (__A__ __B__))
      (body (
        Case
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
            (Var Y)))))))))
  |}];

  (*
   * fun (x) -> y  end
   *)
  print (ExprFun {line=1; name=None; clauses=[
    ClsFun {line=1; patterns=[PatLit {lit=LitAtom {line=1; atom="x"}}]; guard_sequence=None; body=ExprLit {lit=LitAtom {line=1; atom="y"}}}
  ]});
  [%expect {|
    (Abs (
      (args (__A__))
      (body (
        Case
        (Tuple ((Var __A__)))
        ((
          ((PatTuple ((PatConstant (Atom x)))) (Constant (Atom true)))
          (Constant (Atom y))))))))
  |}];


  (*
   * fun (1) -> 2  end
   *)
  print (ExprFun {line=1; name=None; clauses=[
    ClsFun {line=1; patterns=[PatLit {lit=LitInteger {line=1; integer=42}}]; guard_sequence=None; body=ExprLit {lit=LitInteger {line=1; integer=43}}}
  ]});
  [%expect {|
    (Abs (
      (args (__A__))
      (body (
        Case
        (Tuple ((Var __A__)))
        ((
          ((PatTuple ((PatConstant (Number 42)))) (Constant (Atom true)))
          (Constant (Number 43))))))))
  |}];


  (*
   * fun ([]) -> [];
   *     ([H|T]) -> T
   * end
   *)
  print (ExprFun {line=1; name=None; clauses=[
    ClsFun {line=1; patterns=[PatNil {line=1}]; guard_sequence=None; body=ExprNil {line=1}};
    ClsFun {line=2; patterns=[PatCons {line=2; head=PatVar {line=2; id="H"}; tail=PatVar {line=2; id="T"}}]; guard_sequence=None; body=ExprVar {line=2; id="T"}}
  ]});
  [%expect {|
    (Abs (
      (args (__A__))
      (body (
        Case
        (Tuple ((Var __A__)))
        ((((PatTuple (PatNil)) (Constant (Atom true))) ListNil)
         (((PatTuple ((
             PatCons
             (PatVar H)
             (PatVar T))))
           (Constant (Atom true)))
          (Var T))))))) |}];

  (*
   * fun ("abc") -> ok end
   *)
  print (ExprFun {line=1; name=None; clauses=[
    ClsFun {line=1; patterns=[PatLit {lit=LitString {line=1; str="abc"}}]; guard_sequence=None; body=ExprLit {lit=LitAtom {line=1; atom="ok"}}}
  ]});
  [%expect {|
    (Abs (
      (args (__A__))
      (body (
        Case
        (Tuple ((Var __A__)))
        ((
          ((PatTuple ((
             PatCons
             (PatConstant (Number 97))
             (PatCons
               (PatConstant (Number 98))
               (PatCons (PatConstant (Number 99)) PatNil)))))
           (Constant (Atom true)))
          (Constant (Atom ok)))))))) |}];

  (*
   * [1,2,3]
   *)
  print (ExprCons {line=1; head=ExprLit {lit=LitInteger {line=1; integer=1}};
                   tail=ExprCons {line=1; head=ExprLit {lit=LitInteger {line=1; integer=2}};
                                  tail=ExprCons {line=1; head=ExprLit {lit=LitInteger {line=1; integer=3}};
                                                 tail=ExprNil {line=1}}}});
  [%expect {|
    (ListCons
      (Constant (Number 1))
      (ListCons (Constant (Number 2)) (ListCons (Constant (Number 3)) ListNil))) |}];

  (*
   * "abc"
   *)
  print (ExprLit {lit=LitString {line=1; str="abc"}});
  [%expect {|
    (ListCons
      (Constant (Number 97))
      (ListCons (Constant (Number 98)) (ListCons (Constant (Number 99)) ListNil))) |}];

  (*
   * A = B = 1
   *)
  print (ExprMatch {line=1; pattern=PatVar {line=1; id="A"}; body=ExprMatch {line=1; pattern=PatVar {line=1; id="B"}; body=ExprLit {lit=LitInteger {line=1; integer=1}}}});
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
  print (ExprFun {line=1; name=None;
                  clauses=[ClsFun {line=1; patterns=[]; guard_sequence=None;
                                   body=ExprBody {exprs=[ExprMatch {line=1; pattern=PatVar {line=1; id="A"}; body=ExprLit {lit=LitInteger {line=1; integer=1}}};
                                                         ExprMatch {line=1; pattern=PatVar {line=1; id="B"}; body=ExprLit {lit=LitInteger {line=1; integer=2}}};
                                                         ExprBinOp {line=1; op="+"; lhs=ExprVar {line=1; id="A"}; rhs=ExprVar {line=1; id="B"}}]}}]});
  [%expect {|
    (Abs (
      (args ())
      (body (
        Case
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
                 (Var B)))))))))))) |}]

let%expect_test "extract_match_expr" =
  let print expr =
    extract_match_expr expr
    |> [%sexp_of: expr_t list]
    |> Expect_test_helpers_kernel.print_s
  in

  (* input: A = B *)
  (* output: A = B *)
  print (ExprMatch {line=1; pattern=PatVar {line=2; id="A"}; body=ExprVar {line=3; id="B"}});
  [%expect {|
    ((
      ExprMatch
      (line 1)
      (pattern (PatVar  (line 2) (id A)))
      (body    (ExprVar (line 3) (id B))))) |}];

  (* input: A = B = C *)
  (* output: B = C, A = B *)
  print (ExprMatch {line=1; pattern=PatVar {line=2; id="A"}; body=ExprMatch {line=3; pattern=PatVar {line=4; id="B"}; body=ExprVar {line=5; id="C"}}});
  [%expect {|
    ((ExprMatch
       (line 3)
       (pattern (PatVar  (line 4) (id B)))
       (body    (ExprVar (line 5) (id C))))
     (ExprMatch
       (line 1)
       (pattern (PatVar  (line 2) (id A)))
       (body    (ExprVar (line 5) (id C))))) |}];

  (* input: ExprBody [A = B = C, A] *)
  (* output: ExprBody [B = C, A = C, A] *)
  print (ExprBody {exprs=[ExprMatch {line=1; pattern=PatVar {line=2; id="A"}; body=ExprMatch {line=3; pattern=PatVar {line=4; id="B"}; body=ExprVar {line=5; id="C"}}};
                          ExprVar {line=6; id="A"}]});
  [%expect {|
    ((
      ExprBody (
        exprs (
          (ExprMatch
            (line 3)
            (pattern (PatVar  (line 4) (id B)))
            (body    (ExprVar (line 5) (id C))))
          (ExprMatch
            (line 1)
            (pattern (PatVar  (line 2) (id A)))
            (body    (ExprVar (line 5) (id C))))
          (ExprVar
            (line 6)
            (id   A)))))) |}];

  (* input: case A = B of C -> D = E = F; G -> H = I = J end *)
  (* output: A = B, case B of C -> E = F, D = F; G -> I = J, H = J end *)
  print (ExprCase {line=1; expr=ExprMatch {line=2; pattern=PatVar {line=3; id="A"}; body=ExprVar {line=4; id="B"}};
                   clauses=[ClsCase {line=5; pattern=PatVar {line=6; id="C"}; guard_sequence=None;
                                     body=ExprMatch {line=7; pattern=PatVar {line=8; id="D"}; body=ExprMatch {line=9; pattern=PatVar {line=10; id="E"}; body=ExprVar {line=11; id="F"}}}};
                            ClsCase {line=5; pattern=PatVar {line=12; id="G"}; guard_sequence=None;
                                     body=ExprMatch {line=13; pattern=PatVar {line=14; id="H"}; body=ExprMatch {line=15; pattern=PatVar {line=16; id="I"}; body=ExprVar {line=17; id="J"}}}}]});
  [%expect {|
    ((ExprMatch
       (line 2)
       (pattern (PatVar  (line 3) (id A)))
       (body    (ExprVar (line 4) (id B))))
     (ExprCase
       (line 1)
       (expr (
         ExprVar
         (line 4)
         (id   B)))
       (clauses (
         (ClsCase
           (line 5)
           (pattern (
             PatVar
             (line 6)
             (id   C)))
           (guard_sequence ())
           (body (
             ExprBody (
               exprs (
                 (ExprMatch
                   (line 9)
                   (pattern (PatVar  (line 10) (id E)))
                   (body    (ExprVar (line 11) (id F))))
                 (ExprMatch
                   (line 7)
                   (pattern (PatVar  (line 8)  (id D)))
                   (body    (ExprVar (line 11) (id F)))))))))
         (ClsCase
           (line 5)
           (pattern (
             PatVar
             (line 12)
             (id   G)))
           (guard_sequence ())
           (body (
             ExprBody (
               exprs (
                 (ExprMatch
                   (line 15)
                   (pattern (PatVar  (line 16) (id I)))
                   (body    (ExprVar (line 17) (id J))))
                 (ExprMatch
                   (line 13)
                   (pattern (PatVar  (line 14) (id H)))
                   (body    (ExprVar (line 17) (id J))))))))))))) |}];

  (* input: [ N = 1 | M = 2 ] *)
  (* output: N = 1, M = 2, [ 1 | 2 ] *)
  print (ExprCons {line=1;
                   head=ExprMatch {line=2; pattern=PatVar {line=3; id="N"}; body=ExprLit {lit=LitInteger {line=4; integer=1}}};
                   tail=ExprMatch {line=5; pattern=PatVar {line=6; id="M"}; body=ExprLit {lit=LitInteger {line=7; integer=2}}}});
  [%expect {|
    ((ExprMatch
       (line 2)
       (pattern (
         PatVar
         (line 3)
         (id   N)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    4)
             (integer 1))))))
     (ExprMatch
       (line 5)
       (pattern (
         PatVar
         (line 6)
         (id   M)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    7)
             (integer 2))))))
     (ExprCons
       (line 1)
       (head (
         ExprLit (
           lit (
             LitInteger
             (line    4)
             (integer 1)))))
       (tail (
         ExprLit (
           lit (
             LitInteger
             (line    7)
             (integer 2))))))) |}];

  (* input: [] *)
  (* output: [] *)
  print (ExprNil {line=1});
  [%expect {| ((ExprNil (line 1))) |}];

  (* TODO *)
  (* input: [ A = B || B <- [C = D = 1], (E = 2) =:= (F = 3) ] *)
  (* output: ? *)

  (* input: fun f/0 *)
  (* output: fun f/0 *)
  print (ExprLocalFunRef {line=1; function_name="f"; arity=0});
  [%expect {|
    ((
      ExprLocalFunRef
      (line          1)
      (function_name f)
      (arity         0))) |}];

  (* input: fun m:f/0 *)
  (* output: fun m:f/0 *)
  print (ExprRemoteFunRef {line=1; module_name=AtomVarAtom {line=2; atom="m"}; function_name=AtomVarAtom {line=3; atom="f"}; arity=IntegerVarInteger {line=4; integer=0}});
  [%expect {|
    ((
      ExprRemoteFunRef
      (line 1)
      (module_name   (AtomVarAtom       (line 2) (atom    m)))
      (function_name (AtomVarAtom       (line 3) (atom    f)))
      (arity         (IntegerVarInteger (line 4) (integer 0))))) |}];

  (* input: fun () -> A = B = C; () -> D = E = F end *)
  (* output: fun () -> B = C, A = C; () -> E = F, D = F end *)
  print (ExprFun {line=1; name=None;
                  clauses=[ClsFun {line=2; patterns=[]; guard_sequence=None; body=ExprMatch {line=3; pattern=PatVar {line=4; id="A"}; body=ExprMatch {line=5; pattern=PatVar {line=6; id="B"}; body=ExprVar {line=7; id="C"}}}};
                           ClsFun {line=8; patterns=[]; guard_sequence=None; body=ExprMatch {line=9; pattern=PatVar {line=10; id="D"}; body=ExprMatch {line=11; pattern=PatVar {line=12; id="E"}; body=ExprVar {line=13; id="F"}}}}]});
  [%expect {|
    ((
      ExprFun
      (line 1)
      (name ())
      (clauses (
        (ClsFun
          (line 2)
          (patterns       ())
          (guard_sequence ())
          (body (
            ExprBody (
              exprs (
                (ExprMatch
                  (line 5)
                  (pattern (PatVar  (line 6) (id B)))
                  (body    (ExprVar (line 7) (id C))))
                (ExprMatch
                  (line 3)
                  (pattern (PatVar  (line 4) (id A)))
                  (body    (ExprVar (line 7) (id C)))))))))
        (ClsFun
          (line 8)
          (patterns       ())
          (guard_sequence ())
          (body (
            ExprBody (
              exprs (
                (ExprMatch
                  (line 11)
                  (pattern (PatVar  (line 12) (id E)))
                  (body    (ExprVar (line 13) (id F))))
                (ExprMatch
                  (line 9)
                  (pattern (PatVar  (line 10) (id D)))
                  (body    (ExprVar (line 13) (id F))))))))))))) |}];

  (* input: f(N = 1, M = 2) *)
  (* output: M = 2, N = 1, f(1, 2) *)
  print (ExprLocalCall {line=1; function_expr=ExprVar {line=2; id="f"}; args=[ExprMatch {line=3; pattern=PatVar {line=4; id="N"}; body=ExprLit {lit=LitInteger {line=6; integer=1}}};
                                                                              ExprMatch {line=7; pattern=PatVar {line=8; id="M"}; body=ExprLit {lit=LitInteger {line=10; integer=2}}}]});
  [%expect {|
    ((ExprMatch
       (line 7)
       (pattern (
         PatVar
         (line 8)
         (id   M)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    10)
             (integer 2))))))
     (ExprMatch
       (line 3)
       (pattern (
         PatVar
         (line 4)
         (id   N)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    6)
             (integer 1))))))
     (ExprLocalCall
       (line 1)
       (function_expr (
         ExprVar
         (line 2)
         (id   f)))
       (args (
         (ExprLit (
           lit (
             LitInteger
             (line    6)
             (integer 1))))
         (ExprLit (
           lit (
             LitInteger
             (line    10)
             (integer 2)))))))) |}];

  (* input: (M = m):(F = f)(A = 1, B = 2) *)
  (* output: M = m, F = f, B = 2, A = 1, m:f(1, 2) *)
  print (ExprRemoteCall {line=1; line_remote=2;
                         module_expr=ExprMatch {line=3; pattern=PatVar {line=4; id="M"}; body=ExprLit {lit=LitAtom {line=5; atom="m"}}};
                         function_expr=ExprMatch {line=6; pattern=PatVar {line=7; id="F"}; body=ExprLit {lit=LitAtom {line=8; atom="f"}}};
                         args=[ExprMatch {line=9; pattern=PatVar {line=10; id="A"}; body=ExprLit {lit=LitInteger {line=11; integer=1}}};
                               ExprMatch {line=12; pattern=PatVar {line=13; id="B"}; body=ExprLit {lit=LitInteger {line=14; integer=2}}}]});
  [%expect {|
    ((ExprMatch
       (line 3)
       (pattern (
         PatVar
         (line 4)
         (id   M)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 5)
             (atom m))))))
     (ExprMatch
       (line 6)
       (pattern (
         PatVar
         (line 7)
         (id   F)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 8)
             (atom f))))))
     (ExprMatch
       (line 12)
       (pattern (
         PatVar
         (line 13)
         (id   B)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    14)
             (integer 2))))))
     (ExprMatch
       (line 9)
       (pattern (
         PatVar
         (line 10)
         (id   A)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    11)
             (integer 1))))))
     (ExprRemoteCall
       (line        1)
       (line_remote 2)
       (module_expr (
         ExprLit (
           lit (
             LitAtom
             (line 5)
             (atom m)))))
       (function_expr (
         ExprLit (
           lit (
             LitAtom
             (line 8)
             (atom f)))))
       (args (
         (ExprLit (
           lit (
             LitInteger
             (line    11)
             (integer 1))))
         (ExprLit (
           lit (
             LitInteger
             (line    14)
             (integer 2)))))))) |}];

  (* input: #{K1 = k1 => V1 = v1, K2 = k2 := V2 = v2} *)
  (* output: K2 = k2, V2 = v2, K1 = k1, V1 = v1, #{k1 => v1, k2 := v2} *)
  print (ExprMapCreation {line=1;
                          assocs=[ExprAssoc {line=2;
                                             key=ExprMatch {line=3; pattern=PatVar {line=4; id="K1"}; body=ExprLit {lit=LitAtom {line=5; atom="k1"}}};
                                             value=ExprMatch {line=6; pattern=PatVar {line=7; id="V1"}; body=ExprLit {lit=LitAtom {line=8; atom="v1"}}}};
                                  ExprAssocExact {line=9;
                                                  key=ExprMatch {line=10; pattern=PatVar {line=11; id="K2"}; body=ExprLit {lit=LitAtom {line=12; atom="k2"}}};
                                                  value=ExprMatch {line=15; pattern=PatVar {line=14; id="V2"}; body=ExprLit {lit=LitAtom {line=13; atom="v2"}}}}]});
  [%expect {|
    ((ExprMatch
       (line 10)
       (pattern (
         PatVar
         (line 11)
         (id   K2)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 12)
             (atom k2))))))
     (ExprMatch
       (line 15)
       (pattern (
         PatVar
         (line 14)
         (id   V2)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 13)
             (atom v2))))))
     (ExprMatch
       (line 3)
       (pattern (
         PatVar
         (line 4)
         (id   K1)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 5)
             (atom k1))))))
     (ExprMatch
       (line 6)
       (pattern (
         PatVar
         (line 7)
         (id   V1)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 8)
             (atom v1))))))
     (ExprMapCreation
       (line 1)
       (assocs (
         (ExprAssoc
           (line 2)
           (key (
             ExprLit (
               lit (
                 LitAtom
                 (line 5)
                 (atom k1)))))
           (value (
             ExprLit (
               lit (
                 LitAtom
                 (line 8)
                 (atom v1))))))
         (ExprAssocExact
           (line 9)
           (key (
             ExprLit (
               lit (
                 LitAtom
                 (line 12)
                 (atom k2)))))
           (value (
             ExprLit (
               lit (
                 LitAtom
                 (line 13)
                 (atom v2)))))))))) |}];

  (* input: (M = #{})#{K1 = k1 => V1 = v1, K2 = k2 => V2 = v2} *)
  (* output: K2 = k2, V2 = v2, K1 = k1, V1 = v1, #{k1 => v1, k2 => v2} *)
  print (ExprMapCreation {line=1;
                          assocs=[ExprAssoc {line=2;
                                             key=ExprMatch {line=3; pattern=PatVar {line=4; id="K1"}; body=ExprLit {lit=LitAtom {line=5; atom="k1"}}};
                                             value=ExprMatch {line=6; pattern=PatVar {line=7; id="V1"}; body=ExprLit {lit=LitAtom {line=8; atom="v1"}}}};
                                  ExprAssoc {line=9;
                                             key=ExprMatch {line=10; pattern=PatVar {line=11; id="K2"}; body=ExprLit {lit=LitAtom {line=12; atom="k2"}}};
                                             value=ExprMatch {line=15; pattern=PatVar {line=14; id="V2"}; body=ExprLit {lit=LitAtom {line=13; atom="v2"}}}}]});
  [%expect {|
    ((ExprMatch
       (line 10)
       (pattern (
         PatVar
         (line 11)
         (id   K2)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 12)
             (atom k2))))))
     (ExprMatch
       (line 15)
       (pattern (
         PatVar
         (line 14)
         (id   V2)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 13)
             (atom v2))))))
     (ExprMatch
       (line 3)
       (pattern (
         PatVar
         (line 4)
         (id   K1)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 5)
             (atom k1))))))
     (ExprMatch
       (line 6)
       (pattern (
         PatVar
         (line 7)
         (id   V1)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 8)
             (atom v1))))))
     (ExprMapCreation
       (line 1)
       (assocs (
         (ExprAssoc
           (line 2)
           (key (
             ExprLit (
               lit (
                 LitAtom
                 (line 5)
                 (atom k1)))))
           (value (
             ExprLit (
               lit (
                 LitAtom
                 (line 8)
                 (atom v1))))))
         (ExprAssoc
           (line 9)
           (key (
             ExprLit (
               lit (
                 LitAtom
                 (line 12)
                 (atom k2)))))
           (value (
             ExprLit (
               lit (
                 LitAtom
                 (line 13)
                 (atom v2)))))))))) |}];

  (* input: (M = #{})#{K1 = k1 => V1 = v1, K2 = k2 := V2 = v2} *)
  (* output: M = #{}, K2 = k2, V2 = v2, K1 = k2, V1 = v2, (#{})#{k1 => v1, k2 => v2} *)
  print (ExprMapUpdate {line=1;
                        map=ExprMatch {line=2; pattern=PatVar {line=3; id="M"}; body=ExprMapCreation {line=4; assocs=[]}};
                        assocs=[ExprAssoc {line=5;
                                           key=ExprMatch {line=6; pattern=PatVar {line=7; id="K1"}; body=ExprLit {lit=LitAtom {line=8; atom="k1"}}};
                                           value=ExprMatch {line=9; pattern=PatVar {line=10; id="V1"}; body=ExprLit {lit=LitAtom {line=11; atom="v1"}}}};
                                ExprAssocExact {line=12;
                                                key=ExprMatch {line=13; pattern=PatVar {line=14; id="K2"}; body=ExprLit {lit=LitAtom {line=15; atom="k2"}}};
                                                value=ExprMatch {line=16; pattern=PatVar {line=17; id="V2"}; body=ExprLit {lit=LitAtom {line=18; atom="v2"}}}}]});
  [%expect {|
    ((ExprMatch
       (line 2)
       (pattern (
         PatVar
         (line 3)
         (id   M)))
       (body (ExprMapCreation (line 4) (assocs ()))))
     (ExprMatch
       (line 13)
       (pattern (
         PatVar
         (line 14)
         (id   K2)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 15)
             (atom k2))))))
     (ExprMatch
       (line 16)
       (pattern (
         PatVar
         (line 17)
         (id   V2)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 18)
             (atom v2))))))
     (ExprMatch
       (line 6)
       (pattern (
         PatVar
         (line 7)
         (id   K1)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 8)
             (atom k1))))))
     (ExprMatch
       (line 9)
       (pattern (
         PatVar
         (line 10)
         (id   V1)))
       (body (
         ExprLit (
           lit (
             LitAtom
             (line 11)
             (atom v1))))))
     (ExprMapUpdate
       (line 1)
       (map (ExprMapCreation (line 4) (assocs ())))
       (assocs (
         (ExprAssoc
           (line 5)
           (key (
             ExprLit (
               lit (
                 LitAtom
                 (line 8)
                 (atom k1)))))
           (value (
             ExprLit (
               lit (
                 LitAtom
                 (line 11)
                 (atom v1))))))
         (ExprAssocExact
           (line 12)
           (key (
             ExprLit (
               lit (
                 LitAtom
                 (line 15)
                 (atom k2)))))
           (value (
             ExprLit (
               lit (
                 LitAtom
                 (line 18)
                 (atom v2)))))))))) |}];

  (* input: (N = 1) + (M = 2) *)
  (* output: N = 1, M = 2, 1 + 2 *)
  print (ExprBinOp {line=1; op="+";
                    lhs=ExprMatch {line=2; pattern=PatVar {line=3; id="N"}; body=ExprLit {lit=LitInteger {line=4; integer=1}}};
                    rhs=ExprMatch {line=5; pattern=PatVar {line=6; id="M"}; body=ExprLit {lit=LitInteger {line=7; integer=2}}}});
  [%expect {|
    ((ExprMatch
       (line 2)
       (pattern (
         PatVar
         (line 3)
         (id   N)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    4)
             (integer 1))))))
     (ExprMatch
       (line 5)
       (pattern (
         PatVar
         (line 6)
         (id   M)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    7)
             (integer 2))))))
     (ExprBinOp
       (line 1)
       (op   +)
       (lhs (
         ExprLit (
           lit (
             LitInteger
             (line    4)
             (integer 1)))))
       (rhs (
         ExprLit (
           lit (
             LitInteger
             (line    7)
             (integer 2))))))) |}];

  (* input: {N = 1, M = 2} *)
  (* output: M = 2, N = 1, {1, 2} *)
  print (ExprTuple {line=1;
                    elements=[ExprMatch {line=2; pattern=PatVar {line=3; id="N"}; body=ExprLit {lit=LitInteger {line=4; integer=1}}};
                              ExprMatch {line=5; pattern=PatVar {line=6; id="M"}; body=ExprLit {lit=LitInteger {line=7; integer=2}}}]});
  [%expect {|
    ((ExprMatch
       (line 5)
       (pattern (
         PatVar
         (line 6)
         (id   M)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    7)
             (integer 2))))))
     (ExprMatch
       (line 2)
       (pattern (
         PatVar
         (line 3)
         (id   N)))
       (body (
         ExprLit (
           lit (
             LitInteger
             (line    4)
             (integer 1))))))
     (ExprTuple
       (line 1)
       (elements (
         (ExprLit (
           lit (
             LitInteger
             (line    4)
             (integer 1))))
         (ExprLit (
           lit (
             LitInteger
             (line    7)
             (integer 2)))))))) |}];

  (* input: A *)
  (* output: A *)
  print (ExprVar {line=1; id="A"});
  [%expect {|
    ((
      ExprVar
      (line 1)
      (id   A))) |}];

  (* input: a *)
  (* output: a *)
  print (ExprLit {lit=LitAtom {line=1; atom="a"}});
  [%expect {|
    ((
      ExprLit (
        lit (
          LitAtom
          (line 1)
          (atom a))))) |}];
