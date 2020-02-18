%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- bench12.erl
%--
%-- AUTHORS: 	 Tamarit-Emartinm
%-- DATE:        2013           
%-- PUBLISHED:   https://github.com/tamarit/edd/blob/master/examples/ternary (2016)
%-- COPYRIGHT:   Bencher: The Program Slicing Benchmark Suite for Erlang
%--              (Universitat Politècnica de València)
%--              http://www.dsic.upv.es/~jsilva/slicing/bencher/
%-- DESCRIPTION
%-- The program performs translations and operations with balanced ternary. Its inputs are
%-- two numbers in balanced ternary representation and a decimal number. It converts the 
%-- three numbers to the oposite representation and performs an operation with their 
%-- balanced ternary representation. The output is a tuple that contains the input numbers
%-- and the result in both representations.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(b15).
-export([main/3]).

main(AS,B,CS) ->
    AT = from_string(AS), A = from_ternary(AT),	
    BT = to_ternary(B), BS = to_string(BT),
    CT = from_string(CS), C = from_ternary(CT),
    RT = mul(AT,sub(BT,CT)),
    R = from_ternary(RT),
    RS = to_string(RT),
    [{AS,A},{BS,B},{CS,C},{RS,R}].

to_string(T) -> [to_char(X) || X <- T].
 
from_string(S) -> [from_char(X) || X <- S].
 
to_char(-1) -> $-;
to_char(0) -> $0;
to_char(1) -> $+.
 
from_char($-) -> -1;
from_char($0) -> 0;
from_char($+) -> 1.
 
to_ternary(N) when N > 0 ->
    to_ternary(N,[]);
to_ternary(N) ->
    neg(to_ternary(-N)).
 
to_ternary(0,Acc) ->
    Acc;
to_ternary(N,Acc) when N rem 3 == 0 ->
    to_ternary(N div 3, [0|Acc]);
to_ternary(N,Acc) when N rem 3 == 1 ->
    to_ternary(N div 3, [1|Acc]);
to_ternary(N,Acc) ->
    X = to_ternary((N+1) div 3, [-1|Acc]).
 
from_ternary(T) -> from_ternary(T,0).
 
from_ternary([],Acc) ->
    Acc;
from_ternary([H|T],Acc) ->
    from_ternary(T,Acc*3 + H).
 
mul(A,B) -> mul(B,A,[]).
 
mul(_,[],Acc) ->
    Acc;
mul(B,[A|As],Acc) ->
    BP = case A of
             -1 -> neg(B);
             0 ->  [0];
             1 ->  B
         end,
    A1 = Acc++[0],
    A2=add(BP,A1),
    mul(B,As,A2).
 
neg(T) -> [ -H || H <- T].
 
sub(A,B) -> 
	add(A,neg(B)).

add(A,B) when length(A) < length(B) ->
    add(lists:duplicate(length(B)-length(A),0)++A,B);
add(A,B) when length(A) > length(B) ->
   add(B,A);
add(A,B) ->
    add(lists:reverse(A),lists:reverse(B),0,[]).
 
add([],[],0,Acc) ->
    Acc;
add([],[],C,Acc) ->
    [C|Acc];
add([A|As],[B|Bs],C,Acc) ->
    [C1,D] = add_util(A+B+C),
    add(As,Bs,C1,[D|Acc]).
 
add_util(-3) -> [-1,0];
add_util(-2) -> [-1,1];
add_util(-1) -> [0,-1];
add_util(3) -> [1,0];
add_util(2) -> [1,-1];
add_util(1) -> [0,1];
add_util(0) -> [0,0].
 
