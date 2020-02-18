-module(code).
-export([function/0]).

function() ->
	recursive(10, [ 1, 2, 3 ]).

recursive(0, L) -> L;
recursive(N, L) ->
	X = recursive(N - 1, L),
	[ 4 | X ].