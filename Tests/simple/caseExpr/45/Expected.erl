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
        _ = _ ->
            X + undef + undef - undef
    end.