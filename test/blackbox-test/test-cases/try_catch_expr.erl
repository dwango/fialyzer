-module(try_catch_expr).

-export([catch_expr/0, expr_try1/0, expr_try2/0, expr_try3/0]).

-spec catch_expr() -> any().
catch_expr() ->
    catch 1 + 2.


-spec expr_try1() -> number().
expr_try1() ->
    try
        1 + 2
    catch
        Err:Stack -> {hoge, Err, Stack}
    end.

-spec expr_try2() -> number().
expr_try2() ->
    try 1 + 2 of
        X -> 2 * X
    catch
        Err:Stack -> {hoge, Err, Stack}
    end.

-spec expr_try3() -> ok.
expr_try3() ->
    try 1 + 2 of
        X -> 2 * X
    catch
        Err:Stack -> {hoge, Err, Stack}
    after
        ok
    end.
