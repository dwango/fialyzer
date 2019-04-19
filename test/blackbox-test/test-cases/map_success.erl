-module(map_success).

-export([a/1, call_a/0, ab/0]).

-spec a(map()) -> any().
a(M) ->
    case M of
        #{a := A} -> A
    end.

-spec call_a() -> ok.
call_a() ->
    A = #{a => a},
    _ = a(A),
    AB = #{a => a, b => b},
    _ = a(AB),
    ok.

-spec ab() -> {any(), any()}.
ab() ->
    ABC = #{a => a, b => b, c => c},
    case ABC of
        #{a := A, b := B} -> {A, B}
    end.
