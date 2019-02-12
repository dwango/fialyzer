-module(fac_error).

-export([fac/1]).

fac(N) ->
    case N of
        0 -> 1;
        _ -> N * fac('foo')
    end.
