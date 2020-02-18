%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench2.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test higher order functions and anonymous
%--              functions.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The benchmark receives two input parameters and executes a call to a function using fun 
%-- expressions. These expressions are stored in variables and are called later.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(b9).
-export([main/2]).
main(A,B)->
	if 
	    A>B -> 
	   		C=f(A,B);
	    true -> 
	    	C=f(B,A)
	end,
	C,
	D=h(fun g/2,A,B),
	D,
	{C,D}.
f(X,Y) -> 
	Z= X-Y,
	W= X+Y, 
	case Z of
		W -> 
			(fun(B,E) -> 
					N=B*E, 
					B+E 
			end)(X,Y);
		_ -> (fun(N) -> 
				N 
			 end)(W)
	end.
g(X,Y) ->
	(fun(A,B) -> 
		C=A-B, 
		A+B 
	end)(X,Y). 
h(F,A,B) ->  
	C=B*2,
	D= if 
		 B>A -> 
		 	B-3;
		 B=<A -> 
		 	A+5
	   end,
	F(A,B).