-module(list_case_expr_error).

-export([main/1]).

main(Xs) ->
  (fun (Xs) ->
     case Xs of
       [X]  -> X and X
      end
    end)([1,2,3]).
