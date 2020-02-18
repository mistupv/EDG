-module(examples).
-export([example1/0]).
-export([example2/0]).

example1() ->
	{Y, Z} = {1, factorial(100)},
	{A, B} = {Y, Z}.

example2() ->
	X = {1, factorial(100)},
	{A, B} = X.

factorial(0) ->
	1;
factorial(N) when N > 0 ->
	N * factorial(N - 1);
factorial(N) when N < 0 ->
	error.