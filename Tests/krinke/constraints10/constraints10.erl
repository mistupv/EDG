-module(constraints10).
-export([function/0]).

function() ->
	A = [ 1, [ 2, 3 ], 4 ],
	B = [ 5, [ 4, 3 | 2 ], 1 ],
	C = foo(A),
	D = foo(B),
	E = [ C, D ],
	Numbers = foo2(E),
	Numbers.

foo(X) ->
	[ Head | Tail ] = X,
	[ [ A, B ] | C ] = Tail,
	{ X, B }.

foo2(A) ->
	[ B | C ] = A,
	{ _, Type1 } = B,
	[ { _, Type2 } | [] ] = C,
	[ Type1, Type2 ].