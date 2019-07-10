
-module(mod_42).
-export([f/1]).

-spec f(input_42) -> ok.
f(input_42) -> mod_41:f(input_41).
