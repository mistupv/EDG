%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench14.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test challenging slicing problems
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program plans a set of difficult slicing problems like unreachable clauses in case
%-- statements or never called function clauses. It receives two inputs of any nature and
%-- processes them with a suit of case statements and function calls to obtain a final 
%-- result.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b19).
-export([main/2]).

main(X,Y) ->
	Z = case X of
		terminate -> 
			"the end";
		{A,B} -> 
			{[A+B,B-A],3};
		{3,C} -> 
			g(C);
		_ -> 
			{20*3,8}
	end,
	T = 2,
	V = f(T)+h(2)+h(3), 
	W = g([X,Y,{X,Y}]), 
	Tuple = {Z,W,V}, 
	Tuple.

g(X) ->
	[_,_,{R,S}] = X,
	case R of
		[1,3] ->
			21;
		[A,B] ->
			(A*B)/9;
		T ->
			T;
		_ -> 
			f(4)
	end. 

f(7) -> 
	L = 2+9,
	F = L*3,
	F+L;
f(4) -> 
	9;
f(2) -> 
	7;
f(X) -> 
	X.

h(X) -> 
	case X of
		2-> 
			j({2,4});
		3->	
			k([4,8]);
		1-> 
			l(107)
	end.

j(A) -> 
	{X,_} = A,
	X.
k(B) -> 
	[H|T] = B, 
	H.
l(C) -> 
	C-1.