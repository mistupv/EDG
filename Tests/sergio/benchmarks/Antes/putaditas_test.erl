-module(putaditas_test).
-export([main/2,g/1]).

main(X,Y) ->
	Z = case X of
		terminate -> "the end";
		{A,B} -> [A+B,B-A];
		{3,C} -> g(C);
		X -> {A,B} = X, f(2)
	end,
	W = g([X,Y,Z]),
	{Z,W}.

g(X) ->
	[_,_,{R,3}] = X,
	case R of
		[1,3] -> 21;
		[A,B] -> (A*B)/9;
		T ->  [H|J]=T, 
			  [K|L]=J,
			  K;
		_ -> h(3)+h(2)
	end. 

f(7) -> {[1,3],3}.

h(X) -> 
	case X of
		2-> j({2,4});
		3->	k([4,8]);
		1-> l(107)
	end.

j(A) -> {X,_} = A, X.
k(B) -> [H|T] = B, T.
l(C) -> C-1.