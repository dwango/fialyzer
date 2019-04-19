-module(case_univ_pattern).

-export([main/0]).

-spec f(any()) -> 0 | 1.
f(X) ->
    case X of
        ok -> 0;
        _ -> 1
    end.

-spec main() -> 0 | 1.
main() -> f(true).
