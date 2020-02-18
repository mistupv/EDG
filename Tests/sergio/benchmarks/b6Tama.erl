-module(b6Tama).
-export([tuples/2]).
tuples(A, B) ->
    C = ft(A, B).
ft({X,Y},{X,Z,W}) -> 
    case Y of 
        W ->
            gt({1,X});
        Y ->
            gt({0,Y});
        _ ->
            U = Z + X,
            gt({U,Z})
    end.
gt({1,2,_}) ->
    undef;
gt({0,_}) ->
    4;
gt(_) ->
    16.

