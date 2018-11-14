-module(fac).

-spec fac(non_neg_integer()) -> pos_integer().
fac(N) -> 
        case N of
                0 -> 1;
                _ -> N * fac("FOO")
        end.

