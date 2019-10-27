
-module(mod_1).
-export([f/1]).

-spec f(input_1) -> ok.
f(input_1) -> mod_0:f(input_0).
