%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench9.erl
%--
%-- AUTHORS: 	 Anonymous
%-- DATE:        2016           
%-- PUBLISHED:   Software specially developed to test the define and spec instruccions.
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program receives a number and a state to switch the stove as inputs. If the state 
%-- is ON and the number is greater than twenty, the program returns a success message,
%-- if it is ON and the number is less than or equal to twenty the program returns an 
%-- error message. The OFF input state always provides a success message.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b11).
-export([main/2]).
-define(ERROR,failure).
-define(ON,enable).
-define(OFF,disable). 

-type num() :: integer().
-type word() :: string().
-type thing() :: atom().
-type switch() :: ?ON | ?OFF.
-type reply() :: word() | ?ERROR.

-spec light(N::num(),L::switch()) -> {'ok',word()} | {'error',thing()}.
light(N,L) -> 
	if 
		N > 20 andalso L==?ON -> 
			{'ok', "Success switching ON"};
	    N =< 20 andalso L==?ON -> 
	    	{'error', ?ERROR};
  	    true -> 
  	   		{'ok', "Success switching OFF"}
	end.

-spec stove(P::num(),L::switch()) -> reply().
stove(P,L) -> 
	if 
		L == ?ON -> 
			{Res,Reply} = light(P,L);
		true -> 
			{Res,Reply} = light(P,?OFF)
	end,
	Reply.

main(N,State) when (N > -480 andalso N =< 520) andalso (State == ?ON orelse State == ?OFF) -> 
	A=stove(N,State).