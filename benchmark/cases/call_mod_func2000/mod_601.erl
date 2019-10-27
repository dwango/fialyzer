
-module(mod_601).
-export([f/1]).

-spec f(input_601) -> ok.
f(input_601) -> mod_600:f(input_600).
