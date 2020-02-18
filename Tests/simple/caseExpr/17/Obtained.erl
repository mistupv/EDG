-module('Obtained').

-export([decision/2]).

decision(X, _) ->
    case undef of
        _ ->
            undef + X
    end.