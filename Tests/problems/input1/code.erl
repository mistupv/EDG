-module(code).
-export([function/0]).

function() ->
	recursive(3, { 0, { 1, { 2, { 3, 4 } } } }).

recursive(0, T) -> T;
recursive(N, { W, X }) ->
	Y = recursive(N - 1, X),
	Y.