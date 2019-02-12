-module(auto_import).

-export([foo/1]).

-spec foo(number()) -> number().
foo(X) ->
    abs(X).
