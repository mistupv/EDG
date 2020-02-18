-module(b18Tama).
-export([main/2]).

main(X, _) ->
    W = g([undef,undef,{X,undef}|undef]).

g(X) ->
    [_,_,{R,_}|_] = X,
    case R of
        [1,3] ->
            21;
        [A,B] ->
            A * B / 9;
        T ->
            T;
        _ ->
            f(4)
    end.

f(7) ->
    L = 2 + 9,
    F = L * 3,
    F + L;
f(4) ->
    9;
f(2) ->
    7;
f(X) ->
    X.

