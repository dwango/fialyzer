-module(fac).

-export([fac/1]).

-spec fac(number()) -> number().
fac(N) ->
    case N of
        0 -> 1;
        _ -> N * fac(N - 1)
    end.
