-module('Expected').

-export([function/1]).

function(X) ->
    if
        X < 5 ->
            undef;
        (5 =< X) and (X =< 10) ->
            undef;
        10 < X ->
            2
    end.