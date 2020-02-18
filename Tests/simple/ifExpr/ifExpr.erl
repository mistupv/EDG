-module(ifExpr).
-export([function/1]).

function(X) ->
	if
		X < 5 -> { 0, X };
		(5 =< X) and (X =< 10) -> 1;
		10 < X -> 2
	end.