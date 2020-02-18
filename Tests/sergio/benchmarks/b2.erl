%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench11.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test non-called funtions.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- This benchmark consists in a function that receives two lists as inputs and calls
%-- another one named fl with a conditional structure. There is also another function 
%-- gl that is not called by lists or fl.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b2).
-export([lists/2]).

lists(A,B) -> 
	C=fl(A,B).
fl([H1|T1],[H2|T2]) -> 
	if	
		H1 >= 3 -> 
			H2;
		true -> 
			[H|_] = T2,
			H1-H  
	end.
gl([H|T]) -> 
	1.