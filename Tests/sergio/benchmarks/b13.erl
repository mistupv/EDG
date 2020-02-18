%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench10.erl
%--
%-- AUTHORS:     Tamarit
%-- DATE:        2016           
%-- PUBLISHED:   https://github.com/tamarit/hackerrank/tree/master/common-divisors (2016)
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang 
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/ 
%-- DESCRIPTION
%-- The program receives a list with pairs of integers and compute the number of common 
%-- divisors of each pair. The program returns a list with the number of common divisors
%-- in the same order as the input pairs.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------

-module(b13).
-export([main/1]).
main(L) ->
  Res = calculate(L).

calculate(List) ->
    calculate(List, []).

calculate([{A,B}|T], Acc) ->
    DA = divs(A),
    DB = divs(B),
    calculate(T, 
        [sets:size(
            sets:intersection(
                sets:from_list(DA), 
                sets:from_list(DB)))
        | Acc]);
calculate([], Acc) ->
    lists:reverse(Acc).

divs(0) -> [];
divs(1) -> [1];
divs(N) -> [1, N] ++ divisors(2,N,math:sqrt(N)).
 
divisors(K,_N,Q) when K > Q -> [];
divisors(K,N,Q) when N rem K =/= 0 -> 
    divisors(K+1,N,Q);
divisors(K,N,Q) when K * K  == N -> 
    [K] ++ divisors(K+1,N,Q);
divisors(K,N,Q) ->
    [K, N div K] ++ divisors(K+1,N,Q).
    