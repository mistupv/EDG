%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench1.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test dead code detection.
%-- DESCRIPTION
%-- This benchmark consists in a function receiving two numbers as inputs and calling
%-- another one with a conditional structure that contains a dead code statement.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(bench1).
-export([numbers/2]).
numbers(A,B) -> 
	C=fn(A,B).
fn(X,Y) ->
	if	
		X>5 -> 
			Z=Y,
			X; 
		true -> 
			X+2
	end.
