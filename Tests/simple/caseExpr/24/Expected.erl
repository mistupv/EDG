-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    case X of
        1 ->
            X + undef - undef
    end.