
-module(mod_422).
-export([f/1]).

-spec f(input_422) -> ok.
f(input_422) -> mod_421:f(input_421).
