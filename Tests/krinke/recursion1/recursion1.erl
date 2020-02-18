-module(recursion1).
-export([function/0]).

function() ->
	X = [ 1, 2, 3 ],
	Y = foo(X),
	Y.

foo([]) ->
	error;
foo([ Head | [] ]) ->
	Head;
foo([ _ | Tail ]) ->
	X = foo(Tail),
	X.

% Shape analysis