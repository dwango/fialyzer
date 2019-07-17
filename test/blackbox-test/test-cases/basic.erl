-module(basic).

-export([ok/0, id/1, ok2/0, lit_int/0, lit_float/0, match_expr/0]).

-spec ok() -> ok.
ok() -> ok.

-spec id(term()) -> term().
id(A) -> A.

-spec ok2() -> ok.
ok2() -> id(ok()).

-spec lit_int() -> integer().
lit_int() -> 123.

-spec lit_float() -> float().
lit_float() -> 1.23.

-spec match_expr() -> number().
match_expr() ->
    A = id(B = 123),
    A + B.
