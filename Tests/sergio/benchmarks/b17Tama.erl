-module(b17Tama).
-export([main/2]).

main(_, _) ->
    V = f(undef) + h(2) + h(3).

f(_) ->
    7.

h(X) ->
    case X of
        2 ->
            j({2,undef});
        3 ->
            k([4|undef]);
        1 ->
            l(107)
    end.

j(A) ->
    {X,_} = A,
    X.

k(B) ->
    [H|_] = B,
    H.

l(C) ->
    C - 1.