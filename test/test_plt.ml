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
            
