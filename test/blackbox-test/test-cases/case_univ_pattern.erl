-module(case_univ_pattern).

-export([main/0]).

f(X) ->
    case X of
        ok -> 0;
        _ -> 1
    end.

main() -> f(true).
