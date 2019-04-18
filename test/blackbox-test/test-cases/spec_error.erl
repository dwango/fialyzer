-module(spec_error).

-export([foo/1, bar/2]).


-spec foo(number()) -> boolean().
foo(111) -> ok.


-spec bar(boolean(), number()) -> ok.
bar(M, N) ->
    _ = M + N,
    ok.
