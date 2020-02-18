%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test2.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test the detection of unreachable
%--              statements in case conditional structure and never matching clauses 
%--              in functions. 
%-- DESCRIPTION
%-- There are three funcions receiving two parameters (numbers, lists and tuples) and 
%-- calling functions with case clauses and other function calls. Some functions have 
%-- clauses that will never match because a previous clause always matches.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(test2).
-export([numbers/2,lists/2,tuples/2]).

numbers(A,B) -> 			
	C=A+B-3,		
	D=fn(A,B,C).
fn(A,B,C) ->
	case A of 
		3 -> A+B;
		_ -> gn(C) 
	end.

gn(0) -> 1;
gn(X) -> 7.

lists(A,B) -> 
	C=fl(A,B).

fl([H1|T1],[H2|T2]) -> 
	if	H1 >= 3 -> H2;
		true -> H1 + gl(T2)  
	end.

gl(0) -> 3;
gl([]) -> 5;
gl([1|_]) -> 0;
gl([H|T]) -> 1;
gl(_) -> 7.

tuples(A,B) ->
	C=ft(A,B),
	D=ht(B,A),
	{C,D}.

ft({X,Y},{X,Z,W}) ->  
	case Y of		 
		W -> gt({1,X});	
		Y -> gt({0,Y});
		_ -> U=Z+X, gt({U,Z})
	end.

gt({1,2,X}) -> X+4;
gt({0,_}) -> 4;
gt(_) -> 16;
gt({X,Y}) -> Y;
gt({}) -> 0;
gt(X) -> element(1,X).

ht({X,Y,Z},{A,B})-> 
	case Z of
		_ -> A+Z*B;
		Y -> X*2+A;
		A -> U=Y+Z, Z+U
	end.
