%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench13.erl
%--
%-- AUTHORS:   Susan Horwitz
%-- DATE:        1990         
%-- PUBLISHED:   Interprocedural slicing using dependence graphs
%--              ACM Transactions on Programming Languages and Systems
%--              Volume 12 Issue 1, Jan. 1990 Pages 26-60 
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program proposed by Susan Horwitz was designed to ilustrate the problem of the 
%-- Weiser's slicing algorithm in interprocedural slicing. The program makes a recursive 
%-- call to add up the first 11 naturals.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b16).
-export([main/0]).

main() -> 
  Sum = 0,
  I = 1,
  {Result,_} = while(Sum,I,11),
  Result.

while(S,I,Top) ->
  if Top /= 0 ->
        NewS = add(S,I),
        NewI = increment(I),
        while(NewS,NewI,Top-1);
      Top == 0 ->
        {S,I}
  end.

add(A,B) -> A+B.
increment(A) -> add(A,1).

