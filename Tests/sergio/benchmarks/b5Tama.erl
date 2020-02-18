-module(b5Tama).
-export([lists/2]).

lists(A, B) ->
    C = fl(A, B).

fl([H1|_], [H2|T2]) ->
    if
        H1 >= 3 ->
            H2;
        true ->
            H1 + gl(T2)
    end.

gl(0) ->
    3;
gl([]) ->
    5;
gl([1|_]) ->
    0;
gl([_|_]) ->
    1;
gl(_) ->
    7.

