%------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------
%-- bench6.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test the detection of unreachable
%--              clauses in case conditional structure and never matching clauses 
%--              in functions. 
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The benchmark consists in a function that receives two tuples as input. It calls 
%-- another two functions that contain a case statement with unreachable clauses. One of 
%-- this functions also make a call to another function with unreachable and unmatcheable 
%-- clauses.
%------------------------------------------------------------------------------------------
%------------------------------------------------------------------------------------------

-module(b6).
-export([tuples/2]).
tuples(A,B) ->
	C=ft(A,B),
	D=ht(B,A),
	{C,D}.
ft({X,Y},{X,Z,W}) ->  
	case Y of		 
		W -> 
			gt({1,X});	
		Y -> 
			gt({0,Y});
		_ -> 
			U=Z+X,
			gt({U,Z})
	end.
gt({1,2,X}) -> 
	X+4;
gt({0,_}) -> 
	4;
gt(_) -> 
	16;
gt({X,Y}) -> 
	Y;
gt({}) -> 
	0;
gt(X) -> 
	element(1,X).
ht({X,Y,Z},{A,B})-> 
	case Z of
		_ -> 
			A+Z*B;
		Y -> 
			X*2+A;
		A -> 
			U=Y+Z, 
			Z+U
	end.