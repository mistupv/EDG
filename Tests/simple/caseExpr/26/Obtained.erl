-module('Obtained').

-export([decision/2]).

decision(X, Y = _) ->
    case X of
        1 ->
            undef - Y
    end.