-module(case175).

-export([main/0]).

f(X) ->
    case X of
        ok -> 0;
        _ -> 1
    end.

main() -> f(true).
