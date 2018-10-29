open Base
open Fialyzer
open Common
open Result

let%expect_test "result_map_m" =
  let print f =
    result_map_m ~f:f
    >>> [%sexp_of: (int list, string) Result.t]
    >>> Expect_test_helpers_kernel.print_s
  in

  print (fun x -> Ok (x * x)) [1; 2; 3];
  [%expect {| (Ok (1 4 9)) |}];

  print (function 1 -> Ok 1 | _ -> Error "Not One") [1; 2; 3];
  [%expect {| (Error "Not One") |}];
