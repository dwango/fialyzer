
-module(mod_32).
-export([f/1]).

-spec f(input_32) -> ok.
f(input_32) -> mod_31:f(input_31).
