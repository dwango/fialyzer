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
    - $ dialyzer --build_plt test01.beam --output_plt test/unit-test/plt/test01.plt
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
             (params ((Number AnyInteger)))
             (ret (
               List
               (elem_type (
                 Number (
                   IntRange
                   (min (Min 0))
                   (max (Max 1114111)))))
               (terminal_type Nil)
               (is_nonempty   false))))
           ())))
         (args ((Number AnyInteger)))
         (forms ())))))
      (callbacks      ())
      (exported_types ()))) |}];

  (*
    The plt file was created by the following:
    - $ erlc +debug_info samples/specs.erl
    - $ dialyzer --build_plt specs.beam --output_plt test/unit-test/plt/specs.plt
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
         ((contracts (((Function (params (Any)) (ret (AtomUnion (ok)))) ())))
          (args (Any))
          (forms ())))
        (((module_name   specs)
          (function_name f_anyidentifier)
          (arity         1))
         ((contracts ((
            (Function (params (AnyIdentifier)) (ret (AtomUnion (ok)))) ())))
          (args (AnyIdentifier))
          (forms ())))
        (((module_name   specs)
          (function_name f_anymap)
          (arity         1))
         ((contracts (((Function (params (AnyMap)) (ret (AtomUnion (ok)))) ())))
          (args (AnyMap))
          (forms ())))
        (((module_name   specs)
          (function_name f_anytuple)
          (arity         1))
         ((contracts (((Function (params (AnyTuple)) (ret (AtomUnion (ok)))) ())))
          (args (AnyTuple))
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
              (ret (AtomUnion (ok))))
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
              (ret (AtomUnion (ok))))
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
              (ret (AtomUnion (ok))))
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
              (ret (AtomUnion (ok))))
            ())))
          (args ((
            Binary
            (unit 0)
            (base 0))))
          (forms ())))
        (((module_name   specs)
          (function_name f_emptymap)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Map
                (map_pairs ())
                (dict (
                  (key   None)
                  (value None))))))
              (ret (AtomUnion (ok))))
            ())))
          (args ((
            Map
            (map_pairs ())
            (dict (
              (key   None)
              (value None))))))
          (forms ())))
        (((module_name   specs)
          (function_name f_identifier)
          (arity         1))
         ((contracts ((
            (Function
              (params ((IdentifierUnion (IPid IPort))))
              (ret (AtomUnion (ok))))
            ())))
          (args ((IdentifierUnion (IPid IPort))))
          (forms ())))
        (((module_name   specs)
          (function_name f_map)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Map
                (map_pairs (
                  (OptionalPair (
                    (key   (AtomUnion (bar)))
                    (value (AtomUnion (bar)))))
                  (MandatoryPair (
                    (key   (AtomUnion (foo)))
                    (value (AtomUnion (foo)))))))
                (dict (
                  (key (
                    Union (
                      AnyAtom
                      None
                      None
                      None
                      None
                      (Number AnyInteger)
                      None
                      None
                      None
                      None)))
                  (value (AtomUnion (bar foo))))))))
              (ret (AtomUnion (ok))))
            ())))
          (args ((
            Map
            (map_pairs (
              (OptionalPair (
                (key   (AtomUnion (bar)))
                (value (AtomUnion (bar)))))
              (MandatoryPair (
                (key   (AtomUnion (foo)))
                (value (AtomUnion (foo)))))))
            (dict (
              (key (
                Union (
                  AnyAtom
                  None
                  None
                  None
                  None
                  (Number AnyInteger)
                  None
                  None
                  None
                  None)))
              (value (AtomUnion (bar foo))))))))
          (forms ())))
        (((module_name   specs)
          (function_name f_tuple)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Tuple (
                  (types (
                    (AtomUnion (foo))
                    (AtomUnion (bar))))
                  (arity 2)
                  (tag (foo))))))
              (ret (AtomUnion (ok))))
            ())))
          (args ((
            Tuple (
              (types (
                (AtomUnion (foo))
                (AtomUnion (bar))))
              (arity 2)
              (tag (foo))))))
          (forms ())))
        (((module_name   specs)
          (function_name f_union)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Union (
                  (AtomUnion (bar foo))
                  None
                  None
                  (IdentifierUnion (IPid IPort))
                  None
                  (Number (IntSet (1 2)))
                  None
                  None
                  None
                  None))))
              (ret (AtomUnion (ok))))
            ())))
          (args ((
            Union (
              (AtomUnion (bar foo))
              None
              None
              (IdentifierUnion (IPid IPort))
              None
              (Number (IntSet (1 2)))
              None
              None
              None
              None))))
          (forms ())))
        (((module_name   specs)
          (function_name f_union2)
          (arity         1))
         ((contracts ((
            (Function
              (params ((
                Union (
                  (AtomUnion (bar foo))
                  None
                  (Function
                    (params ((Number (IntSet (0 1 2 3 4 5 6 7 8 9 10 11 12)))))
                    (ret (
                      Number (
                        IntRange
                        (min (Min 0))
                        (max (Max 255))))))
                  None
                  None
                  (Number (IntSet (42)))
                  (NTuplesUnion (
                    ((n 1)
                     (tuples ((
                       (types ((Number (IntSet (1 2))))) (arity 1) (tag ())))))
                    ((n 2)
                     (tuples ((
                       (types (
                         (Number (IntSet (1 2)))
                         (Number (IntSet (1 2)))))
                       (arity 2)
                       (tag ())))))))
                  None
                  None
                  None))))
              (ret (AtomUnion (ok))))
            ())))
          (args ((
            Union (
              (AtomUnion (bar foo))
              None
              (Function
                (params ((Number (IntSet (0 1 2 3 4 5 6 7 8 9 10 11 12)))))
                (ret (
                  Number (
                    IntRange
                    (min (Min 0))
                    (max (Max 255))))))
              None
              None
              (Number (IntSet (42)))
              (NTuplesUnion (
                ((n 1)
                 (tuples (((types ((Number (IntSet (1 2))))) (arity 1) (tag ())))))
                ((n 2)
                 (tuples ((
                   (types (
                     (Number (IntSet (1 2)))
                     (Number (IntSet (1 2)))))
                   (arity 2)
                   (tag ())))))))
              None
              None
              None))))
          (forms ())))))
      (callbacks      ())
      (exported_types ()))) |}];
