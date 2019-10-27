
-module(mod_404).
-export([f/1]).

-spec f(input_404) -> ok.
f(input_404) -> mod_403:f(input_403).
