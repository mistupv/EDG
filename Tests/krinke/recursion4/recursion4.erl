-module(recursion4).
-export([function/0]).

function() ->
	X = seq(10, 20),
	X.

seq(First, Last) when First-1 =< Last -> 
    seq_loop(Last-First+1, Last, []).

%seq_loop(N, X, L) when N >= 4 ->
%     seq_loop(N-4, X-4, [X-3,X-2,X-1,X|L]);
seq_loop(N, X, L) when N >= 2 ->
     seq_loop(N-2, X-2, [X-1,X|L]);
seq_loop(1, X, L) ->
     [X,L|0].
%seq_loop(0, _, L) ->
%     L.