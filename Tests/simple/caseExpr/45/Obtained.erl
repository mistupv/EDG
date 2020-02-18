-module('Obtained').

-export([decision/2]).

decision(X, _) ->
    case undef of
        _ ->
            X + undef + undef - undef
    end.