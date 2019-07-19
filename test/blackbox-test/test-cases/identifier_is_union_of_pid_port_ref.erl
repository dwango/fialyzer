-module(identifier_is_union_of_pid_port_ref).
-export([main/0]).

-spec gen_ident1() -> identifier().
gen_ident1() ->
    self().

-spec gen_ident2() -> identifier().
gen_ident2() ->
    list_to_port("test").

-spec gen_ident3() -> identifier().
gen_ident3() ->
    make_ref().

main() ->
    gen_ident1(),
    gen_ident2(),
    gen_ident3().
