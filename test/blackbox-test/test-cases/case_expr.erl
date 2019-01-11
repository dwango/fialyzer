-module(case_expr).

-export([main/1]).

main(X) ->
    case X of
        1 -> 'foo';
        ok -> 123
    end.
