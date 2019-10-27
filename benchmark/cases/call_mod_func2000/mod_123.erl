
-module(mod_123).
-export([f/1]).

-spec f(input_123) -> ok.
f(input_123) -> mod_122:f(input_122).
