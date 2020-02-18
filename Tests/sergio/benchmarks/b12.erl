%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench15.erl
%--
%-- AUTHORS: 	 Tamarit
%-- DATE:        2015           
%-- PUBLISHED:   http://rosettacode.org/wiki/Combinations_and_permutations#Erlang (2016)
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program implements the procedure for making permutations and combinations in an 
%-- interval of given numbers. The inputs are the first and last numbers of the 
%-- permutation interval and the first and last numbers of the combination interval. The 
%-- output will be a set of the permutations and combinations calculated.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b12).
-export([main/4]).
 
perm(N, K) ->
    product(lists:seq(N - K + 1, N)).
 
comb(N, K) ->
    perm(N, K) div product(lists:seq(1, K)).
 
product(List) ->
    lists:foldl(fun(N, Acc) -> N * Acc end, 1, List).

main(PFrom,PTo,CFrom,CTo) ->
    IncremP = if (PTo - PFrom >= 10) -> (PTo-PFrom) div 10;
                true -> 1
              end,
    IncremC = if (CTo - CFrom >= 10) -> (CTo-CFrom) div 10;
                true -> 1
              end,
    io:format("\nPermutations from ~p to ~p:\n",[PFrom,PTo]),
    L1=[show_perm({N, N div 3}) || N <- lists:seq(PFrom, PTo, IncremP)],
    io:format("\nCombinations from ~p to ~p:\n",[CFrom,CTo]),
    L2=[show_comb({N, N div 3}) || N <- lists:seq(CFrom, CTo, IncremC)],
    {L1,L2}.
 
show_perm({N, K}) ->
    show_gen(N, K, "perm", fun perm/2).
 
show_comb({N, K}) ->
    show_gen(N, K, "comb", fun comb/2).
 
show_gen(N, K, StrFun, Fun) ->
    io:format("~s(~p, ~p) = ~s\n",[StrFun, N, K, show_big(Fun(N, K), 40)]).
 
show_big(N, Limit) ->
    StrN = integer_to_list(N),
    case length(StrN) < Limit of
        true ->
            StrN;
        false -> 
            {Shown, Hidden} = lists:split(Limit, StrN),
            io_lib:format("~s... (~p more digits)", [Shown, length(Hidden)]) 
    end. 
