
-module(mod_1900).
-export([f/1]).

-spec f(input_1900) -> ok.
f(input_1900) -> mod_1899:f(input_1899).
