-module(b8Tama).
-export([main/2]).
main(A, B) ->
    if
        A > B ->
            C = f(A, B);
        true ->
            C = f(B, A)
    end,
    C,
    _ = h(fun g/2, A, B).

f(X, Y) ->
    Z = X - Y,
    W = X + Y,
    case Z of
        W ->
            fun(B, E) ->
                   B + E
            end(X, Y);
        _ ->
            fun(N) ->
                   N
            end(W)
    end.

g(X, Y) ->
        fun(A,B) -> 
                A + B 
        end(X, Y). 

h(F, A, B) ->
    F(A, B).
