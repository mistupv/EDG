%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test2Slice1.erl
%--
%-- DATE:        2016 
%-- SLICING CRITERION: Variable D (LINE 22)         
%-- DESCRIPTION
%-- This program is a theoric result of applying static backward slicing to variable D at
%-- line 22 of the test2.erl file.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(code).
-export([numbers/2]).

numbers(A,B) -> 
	F = 2,		
	C=A+B-3,		
	D=fn(A,B,C),slice_void(D).
fn(A,B,C) ->
	case A of 
		3 -> A+B;
		_ -> gn(C) 
	end.

gn(0) -> 1;
gn(_) -> 7.


slice(X) ->
    {ok,Fd}=file:open(?MODULE_STRING++".txt",[append]),
    io:format(Fd,"~p\n",[X]),
    file:close(Fd), X.

slice_void(X)->
    {ok,Fd}=file:open(?MODULE_STRING++".txt",[append]),
    io:format(Fd,"~p\n",[X]),
    file:close(Fd).