-module(b16Tama).
-export([main/0]).

main() ->
    I = 1,
    _ = while(undef, I, 11).

while(_, I, Top) ->
    if
        Top /= 0 ->
            NewI = increment(I),
            while(undef, NewI, Top - 1)
    end.

add(A, B) ->
    A + B.

increment(A) ->
    add(A, 1).