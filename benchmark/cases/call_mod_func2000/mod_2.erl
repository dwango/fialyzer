
-module(mod_2).
-export([f/1]).

-spec f(input_2) -> ok.
f(input_2) -> mod_1:f(input_1).
