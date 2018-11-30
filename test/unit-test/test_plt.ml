open Base
open Fialyzer
module Etf = Obeam.External_term_format
module E = Etf_util

let%expect_test "Plt.of_file" =
  let print filename =
    Plt.of_file filename
    |> [%sexp_of: (Plt.t, exn) Result.t]
    |> Expect_test_helpers_kernel.print_s
  in

  (*
    The plt file was created by the following:
    - $ erlc +debug_info samples/test01.erl
    - $ dialyzer --build_plt test01.beam --output_plt plt/test01.plt
   *)
  print "plt/test01.plt";
  [%expect {|
    (Ok (
      (info  ())
      (types ())
      (contracts ((
        (test01 h 1)
        ((contracts ((
           (Function
             ((Number IntegerQual))
             (List (Number IntegerQual) Nil UnknownQual))
           ())))
         (args ((Number IntegerQual)))
         (forms ())))))
      (callbacks      ())
      (exported_types ()))) |}];

  (*
    The plt file was created by the following:
    - $ erlc +debug_info samples/specs.erl
    - $ dialyzer --build_plt specs.beam --output_plt plt/specs.plt
   *)
  print "plt/specs.plt";
  [%expect {|
    (Ok (
      (info  ())
      (types ())
      (contracts (
        ((specs f_any 1)
         ((contracts (((Function (Any) (Atom (ok))) ()))) (args (Any)) (forms ())))
        ((specs f_binary01 1)
         ((contracts (((Function ((Binary 1 0)) (Atom (ok))) ())))
          (args ((Binary 1 0)))
          (forms ())))
        ((specs f_binary02 1)
         ((contracts (((Function ((Binary 8 0)) (Atom (ok))) ())))
          (args ((Binary 8 0)))
          (forms ())))
        ((specs f_binary03 1)
         ((contracts (((Function ((Binary 2222 1111)) (Atom (ok))) ())))
          (args ((Binary 2222 1111)))
          (forms ())))
        ((specs f_binary04 1)
         ((contracts (((Function ((Binary 0 0)) (Atom (ok))) ())))
          (args ((Binary 0 0)))
          (forms ())))))
      (callbacks      ())
      (exported_types ()))) |}];
