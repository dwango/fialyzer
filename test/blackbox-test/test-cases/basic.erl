-module(basic).

-export([ok/0, id/1, ok2/0]).

-spec ok() -> ok.
ok() -> ok.

-spec id(term()) -> term().
id(A) -> A.

-spec ok2() -> ok.
ok2() -> id(ok()).
