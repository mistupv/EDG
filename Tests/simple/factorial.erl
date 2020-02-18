-module(factorial).
-export([factorial/1]).

factorial(N) ->
	case N of
		X when X < 0 -> error;
		0 -> 1;
		_ -> N * factorial(N - 1)
	end.