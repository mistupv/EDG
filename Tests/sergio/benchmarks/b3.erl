%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench3.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test unreachable clauses.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- This benchmark contains a function that receives two tuples with two elements as 
%-- inputs and calls another one named ft with two condicional statements. Note that 
%-- the third clause of the second conditional statement is unreachable.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(b3).
-export([tuples/2]).
tuples(A,B) -> 
	C=ft(A,B).
ft({X1,X2},{Y1,Y2}) ->
	if  
		X1 =< Y2 -> 
			R = X1;
		true -> 
			R = X2 
	end,
	if	
		X1 > X2 -> 
			Z= Y2 + 3,
			S = 5;
		true -> 
			S = X2;
		X2 < 7 -> 
			S=53
	end,
	S.