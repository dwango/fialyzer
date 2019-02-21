-module(map_success).

-export([a/1, call_a/0, ab/0]).

a(M) ->
    case M of
        #{a := A} -> A
    end.

call_a() ->
    A = #{a => a},
    _ = a(A),
    AB = #{a => a, b => b},
    _ = a(AB),
    ok.

ab() ->
    ABC = #{a => a, b => b, c => c},
    case ABC of
        #{a := A, b := B} -> {A, B}
    end.
