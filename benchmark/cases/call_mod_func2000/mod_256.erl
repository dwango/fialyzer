
-module(mod_256).
-export([f/1]).

-spec f(input_256) -> ok.
f(input_256) -> mod_255:f(input_255).
