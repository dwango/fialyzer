
-module(mod_401).
-export([f/1]).

-spec f(input_401) -> ok.
f(input_401) -> mod_400:f(input_400).
