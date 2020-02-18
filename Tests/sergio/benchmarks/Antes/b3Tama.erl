-module(b3Tama).
-export([tuples/2]).
tuples(A, _) ->
    C = ft(A, undef).

ft({X1,X2}, {_,_}) ->
    if
        X1 > X2 ->
            S = 5;
        true ->
            S = X2;
        X2 < 7 ->
            S = 53
    end,
    S.