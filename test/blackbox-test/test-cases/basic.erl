-module(basic).

-export([ok/0, id/1, ok2/0]).

ok() -> ok.

id(A) -> A.

ok2() -> id(ok()).
