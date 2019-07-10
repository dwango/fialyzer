
-module(mod_500).
-export([f/1]).

-spec f(input_500) -> ok.
f(input_500) -> mod_499:f(input_499).
