%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench5.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test the detection of unreachable
%--              clauses in functions due to a previous clause always matches or another 
%--              never matches. In this case, the slicer need to notice the structure of
%--              the input (a list) to avoid clauses.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The benchmark has a function that receives two lists as inputs. These lists are 
%-- processed in a call using their elements in a case statement. Another function can 
%-- be called in the case using as input the tail of one of the lists.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(b5).
-export([lists/2]).
lists(A,B) -> 
	C=fl(A,B).
fl([H1|T1],[H2|T2]) -> 
	if	
		H1 >= 3 -> 
			H2;
		true -> 
			H1 + gl(T2)  
	end.
gl(0) -> 
	3;
gl([]) -> 
	5;
gl([1|_]) -> 
	0;
gl([H|T]) -> 
	1;
gl(_) -> 
	7.