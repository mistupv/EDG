-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    case X of
        _ ->
            undef;
        1 ->
            undef;
        2 ->
            undef;
        Z = _ ->
            undef + Z - undef
    end.