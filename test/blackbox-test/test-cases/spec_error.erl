-module(spec_error).

-export([foo/1, int1/0, int2/0, int3/0, bar/2]).


-spec foo(number()) -> boolean().
foo(111) -> ok.

-spec int1() -> non_neg_integer().
int1() -> ok.

-spec int2() -> pos_integer().
int2() -> ok.

-spec int3() -> neg_integer().
int3() -> ok.

-spec bar(boolean(), number()) -> ok.
bar(M, N) ->
    _ = M + N,
    ok.
