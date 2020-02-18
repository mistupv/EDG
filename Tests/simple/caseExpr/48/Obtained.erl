-module('Obtained').

-export([decision/2]).

decision(_, _ = D) ->
    case undef of
        _ ->
            undef - D
    end.