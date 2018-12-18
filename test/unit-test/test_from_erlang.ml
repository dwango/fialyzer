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
