open Base
open Fialyzer
open Type

let%expect_test "pp" =
  let print ty = pp ty |> Caml.print_string in

  print TyAny;
  [%expect {| any() |}];

  print TyBottom;
  [%expect {| none() |}];

  print TyAtom;
  [%expect {| atom() |}];

  print TyNumber;
  [%expect {| number() |}];

  print (TySingleton (Number 1));
  [%expect {| 1 |}];

  print (TySingleton (Atom "ok"));
  [%expect {| 'ok' |}];

  (* single union *)
  print (TyUnion (TyAtom, TyNumber));
  [%expect {| atom() | number() |}];

  (* multi union *)
  print (TyUnion (TyUnion (TyUnion (TySingleton (Number 1), TySingleton (Number 2)), TySingleton (Number 3)),
                  TyUnion (TyUnion (TySingleton (Number 4), TySingleton (Number 5)), TySingleton (Number 6))));
  [%expect {| 1 | 2 | 3 | 4 | 5 | 6 |}];

  print (TyVar (Type_variable.of_string "T"));
  [%expect {| T |}];

  print (TyTuple [TyAny; TyTuple [TyBottom; TyBottom]]);
  [%expect {| {any(), {none(), none()}} |}];

  (* fun with no args *)
  print (TyFun ([], TySingleton (Atom "ok")));
  [%expect {| fun(() -> 'ok') |}];

  (* fun with some args *)
  print (TyFun ([TyNumber; TyNumber], TyNumber));
  [%expect {| fun((number(), number()) -> number()) |}];

  (* complex example *)
  print (TyFun ([TyFun ([TyNumber; TyAtom], TyVar (Type_variable.of_string "A")); TyTuple [TyNumber; TyAtom]],
                TyUnion (TyTuple [TySingleton (Atom "ok"); TyVar (Type_variable.of_string "A")],
                         TyTuple [TySingleton (Atom "error"); TyAny])));
  [%expect {| fun((fun((number(), atom()) -> A), {number(), atom()}) -> {'ok', A} | {'error', any()}) |}];
