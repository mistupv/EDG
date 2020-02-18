-module('Expected').

-export([decision/2]).

decision(X, _ = _) ->
    case undef of
        _ ->
            X + undef
    end.