-module(test20).
-export([main/1]).
main(L) ->
  Res = calculate(L).

calculate(List) ->
    calculate(List, []).

calculate([[A,B]|T], Acc) ->
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