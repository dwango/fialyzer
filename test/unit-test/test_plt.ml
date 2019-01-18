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
        ((module_name   test01)
         (function_name h)
         (arity         1))
        ((contracts ((
           (Function
             (params ((Number Integer)))
             (ret (
               List
               (elem_type (
                 Number (
                   IntRange
                   (min (Min 0))
                   (max (Max 1114111)))))
               (term_type   Nil)
               (is_nonempty false))))
           ())))
         (args ((Number Integer)))
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
        (((module_name   specs)
          (function_name f_any)
          (arity         1))
         ((contracts ((
            (Function
              (params (Any))
              (ret (Atom (atoms_union_or_any_atom (AtomsUnion (ok))))))
            ())))
          (args (Any))
          (forms ())))
        (((module_name   specs)
          (function_name f_binary01)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Binary
                (unit 1)
                (base 0))))
              (ret (Atom (atoms_union_or_any_atom (AtomsUnion (ok))))))
            ())))
          (args ((
            Binary
            (unit 1)
            (base 0))))
          (forms ())))
        (((module_name   specs)
          (function_name f_binary02)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Binary
                (unit 8)
                (base 0))))
              (ret (Atom (atoms_union_or_any_atom (AtomsUnion (ok))))))
            ())))
          (args ((
            Binary
            (unit 8)
            (base 0))))
          (forms ())))
        (((module_name   specs)
          (function_name f_binary03)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Binary
                (unit 2222)
                (base 1111))))
              (ret (Atom (atoms_union_or_any_atom (AtomsUnion (ok))))))
            ())))
          (args ((
            Binary
            (unit 2222)
            (base 1111))))
          (forms ())))
        (((module_name   specs)
          (function_name f_binary04)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Binary
                (unit 0)
                (base 0))))
              (ret (Atom (atoms_union_or_any_atom (AtomsUnion (ok))))))
            ())))
          (args ((
            Binary
            (unit 0)
            (base 0))))
          (forms ())))))
      (callbacks      ())
      (exported_types ()))) |}];
