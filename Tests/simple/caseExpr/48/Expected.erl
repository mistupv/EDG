-module('Expected').

-export([decision/2]).

decision(X, _ = D) ->
    case X of
        _ ->
            undef;
        1 ->
            undef;
        2 ->
            undef;
        _ = _ ->
            undef - D
    end.