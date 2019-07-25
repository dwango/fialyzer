-module(identifier_is_union_of_pid_port_ref_error).
-export([main/0]).

-spec wrong_gen_ident() -> identifier().
wrong_gen_ident() ->
    false.

main() ->
    wrong_gen_ident().
