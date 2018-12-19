%% from https://learnyousomeerlang.com/dialyzer#type-inference-and-discrepancies
-module(discrep1).
-export([run/0]).

run() -> some_op(5, you).

some_op(A, B) -> A + B.
