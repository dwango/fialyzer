-module(test02).

-spec i(A) -> integer() when A :: integer().
i(N) -> N.

-spec j(A) -> B when A :: integer(),
                     B :: integer().
j(N) ->
    N * N.
