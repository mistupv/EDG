-module(constraints12).
-export([function/0]).

function() ->
	X = [ 1, 2, 3 ],
	Y = foo(X),
	Y.

foo([ Head | [ Head2 | Tail ] ]) ->
	1.