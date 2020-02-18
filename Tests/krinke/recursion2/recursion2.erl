-module(recursion2).
-export([function/0]).

function() ->
	X = [ 1, 2, 3 ],
	Y = foo(X, 0, 1, 3),
	Y.

foo([ Head | Tail ], Index, SearchIndex, Top) ->
	if
		Tail == []; Index >= Top ->
			error;
		Index == SearchIndex ->
			Head;
		true ->
			foo(Tail, Index + 1, SearchIndex, Top)
	end;
foo(_, _, _, _) ->
	error.