-module('Obtained').

-export([decision/2]).

decision(_, Y = _) ->
    case undef of
        _ ->
            undef + Y + undef - undef
    end.