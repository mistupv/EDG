-module(b11Tama).
-export([main/2]).

light(N, L) ->
    if
        N > 20
        andalso
        L == enable ->
            {ok,"Success switching ON"};
        N =< 20
        andalso
        L == enable ->
            {error,failure};
        true ->
            {ok,"Success switching OFF"}
    end.

stove(P, L) ->
    if
        L == enable ->
            {_,Reply} = light(P, L);
        true ->
            {_,Reply} = light(P, disable)
    end,
    Reply.

main(N, State)
    when
        N >= - 480
        andalso
        N =< 520 ->
    A = stove(N, State).
