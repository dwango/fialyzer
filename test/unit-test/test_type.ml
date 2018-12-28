open Base
open Fialyzer
open Type

let%expect_test "pp" =
  let print ty = pp ty |> Caml.print_string in

  print TyAny;
  [%expect {| any() |}];

  print TyBottom;
  [%expect {| none() |}];

  print (of_elem TyAtom);
  [%expect {| atom() |}];

  print (of_elem TyNumber);
  [%expect {| number() |}];

  print (of_elem (TySingleton (Number 1)));
  [%expect {| 1 |}];

  print (of_elem (TySingleton (Atom "ok")));
  [%expect {| 'ok' |}];

  (* single union *)
  print (TyUnion [TyAtom; TyNumber]);
  [%expect {| atom() | number() |}];

  (* multi union *)
  print (TyUnion [ TySingleton (Number 1); TySingleton (Number 2); TySingleton (Number 3);
                   TySingleton (Number 4); TySingleton (Number 5); TySingleton (Number 6); ]);
  [%expect {| 1 | 2 | 3 | 4 | 5 | 6 |}];

  print (of_elem (TyVar (Type_variable.of_string "T")));
  [%expect {| T |}];

  print (of_elem (TyTuple [TyAny; of_elem (TyTuple [TyBottom; TyBottom])]));
  [%expect {| {any(), {none(), none()}} |}];

  (* fun with no args *)
  print (of_elem (TyFun ([], (of_elem (TySingleton (Atom "ok"))))));
  [%expect {| fun(() -> 'ok') |}];

  (* fun with some args *)
  print (of_elem (TyFun ([of_elem TyNumber; of_elem TyNumber], of_elem TyNumber)));
  [%expect {| fun((number(), number()) -> number()) |}];

  (* complex example *)
  print (of_elem (TyFun (
                      [
                        of_elem (TyFun ([of_elem TyNumber; of_elem TyAtom], of_elem (TyVar (Type_variable.of_string "A"))));
                        of_elem (TyTuple [of_elem TyNumber; of_elem TyAtom])
                      ],
                      TyUnion [TyTuple [of_elem (TySingleton (Atom "ok")); of_elem (TyVar (Type_variable.of_string "A"))];
                               TyTuple [of_elem (TySingleton (Atom "error")); TyAny]])));
  [%expect {| fun((fun((number(), atom()) -> A), {number(), atom()}) -> {'ok', A} | {'error', any()}) |}]

let%expect_test "inf" =
  let print ty1 ty2 =
    inf ty1 ty2
    |> [%sexp_of: typ]
    |> Expect_test_helpers_kernel.print_s in

  print TyAny TyAny;
  [%expect {| TyAny |}];

  print TyBottom TyAny;
  [%expect {| TyBottom |}];

  print (of_elem (TySingleton (Number 1))) (of_elem (TySingleton (Number 2)));
  [%expect {| TyBottom |}];

  print (TyUnion [TySingleton (Number 1); TySingleton (Number 2); TySingleton (Number 3); ])
        (TyUnion [TySingleton (Number 2); TySingleton (Number 3); TySingleton (Number 4); ]);
  [%expect {|
    (TyUnion (
      (TySingleton (Number 3))
      (TySingleton (Number 2))))
 |}];

  print (TyUnion [TySingleton (Number 1); TySingleton (Number 2); TySingleton (Number 3);]) (of_elem TyNumber);
  [%expect {|
    (TyUnion (
      (TySingleton (Number 3))
      (TySingleton (Number 2))
      (TySingleton (Number 1))))
            |}]
