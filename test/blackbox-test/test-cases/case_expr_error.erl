-module(case_expr_error).

-export([main/1]).

main(X) ->
    case X of
        1 -> 'foo' + 42;
        ok -> 123
    end.
