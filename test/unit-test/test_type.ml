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
    |> [%sexp_of: Type.t]
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

let%expect_test "sup" =
  let print ty1 ty2 =
    sup ty1 ty2
    |> [%sexp_of: Type.t]
    |> Expect_test_helpers_kernel.print_s in

  print TyAny TyAny;
  [%expect {| TyAny |}];

  print TyBottom TyAny;
  [%expect {| TyAny |}];

  print (of_elem (TySingleton (Number 1))) (of_elem (TySingleton (Number 2)));
  [%expect {|
    (TyUnion (
      (TySingleton (Number 1))
      (TySingleton (Number 2)))) |}];

  print (TyUnion [TySingleton (Number 1); TySingleton (Number 2); TySingleton (Number 3); ])
        (TyUnion [TySingleton (Number 2); TySingleton (Number 3); TySingleton (Number 4); ]);
  [%expect {|
    (TyUnion (
      (TySingleton (Number 1))
      (TySingleton (Number 2))
      (TySingleton (Number 3))
      (TySingleton (Number 4)))) |}];

  print (TyUnion [TySingleton (Number 1); TySingleton (Number 2); TySingleton (Number 3);]) (of_elem TyNumber);
  [%expect {| (TyUnion (TyNumber)) |}];

  (*
   * sup of function types
   * see: https://github.com/dwango/fialyzer/issues/177
   *)
  let ty1 = of_elem (TyFun ([of_elem TyNumber], of_elem TyNumber)) in
  let ty2 = of_elem (TyFun ([of_elem TyAtom], of_elem TyAtom)) in
  print ty1 ty2;
  [%expect {|
     (TyUnion ((TyFun ((TyUnion (TyAtom TyNumber))) (TyUnion (TyNumber TyAtom)))))
  |}];

  ()

let%expect_test "of_erl_type" =
  let {Plt.contracts; _} = Result.ok_exn (Plt.of_file "plt/specs.plt") in

  let print function_name =
    let erl_type =
      match Map.find_exn contracts Mfa.{module_name="specs"; function_name; arity=1} with
      | {Plt.contracts=[(erl_type, _)]; _} -> erl_type
      | _ -> failwith "unexpected error"
    in
    of_erl_type erl_type
    |> [%sexp_of: Type.t]
    |> Expect_test_helpers_kernel.print_s in

  print "f_any"; (* (any()) -> ok *)
  [%expect {| (TyUnion ((TyFun (TyAny) (TyUnion ((TySingleton (Atom ok))))))) |}];

  print "f_tuple"; (* ({foo, bar}) -> ok *)
  [%expect {|
    (TyUnion ((
      TyFun
      ((
        TyUnion ((
          TyTuple (
            (TyUnion ((TySingleton (Atom foo))))
            (TyUnion ((TySingleton (Atom bar)))))))))
      (TyUnion ((TySingleton (Atom ok))))))) |}];

  print "f_union2"; (* (foo | bar | fun((0..12) -> 0..255) | 42 | {1} | {2} | {1, 1} | {2, 2}) -> ok *)
  [%expect {|
    (TyUnion ((
      TyFun
      ((
        TyUnion (
          (TySingleton (Atom bar))
          (TySingleton (Atom foo))
          (TyFun ((TyUnion (TyNumber))) (TyUnion (TyNumber)))
          (TySingleton (Number 42))
          (TyTuple ((TyUnion (TyNumber))))
          (TyTuple (
            (TyUnion (TyNumber))
            (TyUnion (TyNumber)))))))
      (TyUnion ((TySingleton (Atom ok))))))) |}];
