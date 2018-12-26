open Base
open Fialyzer
open Ast
open From_erlang
open Obeam.Abstract_format

let%expect_test "from_erlang" =
  let print abstract_format =
    Variable.reset_count ();
    expr_of_erlang_expr abstract_format
    |> [%sexp_of: expr]
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
