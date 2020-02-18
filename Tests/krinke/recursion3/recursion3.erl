-module(recursion3).
-export([function/0]).

function() ->
	X = [ 1, 2, 3, 4, 5 ],
	Y = foo(X),
	Y.

%foo([]) ->
%	error;
foo([ Head | [] ]) ->
	Head;
foo([ _ | Tail ]) ->
	X = foo(Tail),
	X.
foo([ _ | [ Head | Tail ] ]) ->
	X = foo(Head),
	X.