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
    - $ dialyzer --build_plt test01.beam --output_plt test01.plt
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
      (exported_types ()))) |}]
