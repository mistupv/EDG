-module('Expected').

-export([decision/2]).

decision(X, Y = _) ->
    case X of
        _ ->
            undef;
        1 ->
            undef;
        2 ->
            undef;
        _ = _ ->
            undef + Y + undef - undef
    end.