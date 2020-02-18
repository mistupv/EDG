%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test1.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test dead code detection and unreachable
%--              code in conditional statements.
%-- DESCRIPTION
%-- There are three funcions receiving two parameters (numbers, lists and tuples)
%-- and calling other functions with if conditional statements.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(test1).
-export([numbers/2,lists/2,tuples/2]).

numbers(A,B) -> 
	C=fn(A,B).

fn(X,Y) ->
	if	X>5 -> Z=Y, X; 
		true -> X+2
	end.


lists(A,B) -> 
	C=fl(A,B).

fl([H1|T1],[H2|T2]) -> 
	if	H1 >= 3 -> H2;
		true -> [H|_]=T2,H1-H  
	end.

gl([H|T]) -> 1.


tuples(A,B) -> 
	C=ft(A,B).

ft({X1,X2},{Y1,Y2}) ->
	if  X1 =< Y2 -> R = X1;
		true -> R = X2 
	end,
	if	X1 > X2 -> Z= Y2 + 3, S = 5;
		true -> S = X2;
		X2 < 7 -> S=53
	end,
	S.
