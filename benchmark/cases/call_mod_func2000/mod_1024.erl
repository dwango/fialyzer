
-module(mod_1024).
-export([f/1]).

-spec f(input_1024) -> ok.
f(input_1024) -> mod_1023:f(input_1023).
