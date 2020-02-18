%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
%-- test14.erl
%--
%-- AUTHORS: 	 Tamarit
%-- DATE:        2015          
%-- PUBLISHED:   http://rosettacode.org/wiki/24_game#Erlang (2016)
%-- DESCRIPTION
%-- This program is a modification of the original program that implements the 24 Game.
%-- The program generates a list of four numbers (in our case the list will always be 
%-- [4,2,3,7]), and the user must use this numbers to achieve 24 operating with them. 
%-- The output will be a message with the result of the operation introduced by the user.
%-----------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------
-module(test14).
-export([main/1]).

main(Exp) ->
    play(Exp).
 
play(Exp) ->
    Digts = [4,2,3,7],
    read_eval(Exp,Digts).
 
read_eval(Exp,Digits) -> 
    case {correct_nums(Exp, Digits), catch eval(Exp)} of
        {ok, X} when X == 24 -> io:format("You Win!~n");
        {ok, X} -> io:format("You Lose with ~p!~n",[X]);
        {List, _} -> 
            io:format("The following numbers are wrong: ~p~n", [List])
    end.
 
correct_nums(Exp, Digits) ->
    case re:run(Exp, "([0-9]+)", [global, {capture, all_but_first, list}]) of
        nomatch ->
            "No number entered";
        {match, IntLs} ->
            case [X || [X] <- IntLs, not lists:member(list_to_integer(X), Digits)] of
                [] -> ok;
                L -> L
            end
    end.
 
eval(Exp) ->
    {X, _} = eval(re:replace(Exp, "\\s", "", [{return, list},global]),
                  0),
    X.
 
eval([], Val) ->
    {Val,[]};
eval([$(|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$)|Rest], Val) ->
    {Val, Rest};
eval([$[|Rest], Val) ->
    {NewVal, Exp} = eval(Rest, Val),
    eval(Exp, NewVal);
eval([$]|Rest], Val) ->
    {Val, Rest};
eval([$+|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val + NewOperand);
eval([$-|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val - NewOperand);
eval([$*|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val * NewOperand);
eval([$/|Rest], Val) ->
    {NewOperand, Exp} = eval(Rest, 0),
    eval(Exp, Val / NewOperand);
eval([X|Rest], 0) when X >= $1, X =< $9 ->
    eval(Rest, X-$0).
