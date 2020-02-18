-module(b7Tama).
-export([tuples/2]).

tuples(A, B) ->
    D = ht(B, A).

ht({X,Y,Z}, {A,B}) ->
    case Z of
        _ ->
            A + Z * B;
        _ ->
            X * 2 + A;
        _ ->
            U = Y + Z,
            Z + U
    end.
