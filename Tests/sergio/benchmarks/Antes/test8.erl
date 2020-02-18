%-----------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------
%-- test8.erl
%--
%-- AUTHORS: 	 Anonymous (o Fred Hébert)
%-- DATE:        2009           
%-- PUBLISHED:   http://learnyousomeerlang.com/recursion (More than lists)(2016)
%--              http://learnyousomeerlang.com/errors-and-exceptions (Try a try in a tree)(2016)
%--              Adaptation of the implementation given in this chapters to record structures.
%-- DESCRIPTION
%-- This program receive a tree, a tuple to be inserted on it and a key and a value to be 
%-- found on it. First of all the program will insert the tuple and then perform the search 
%-- of the given values. The output will be a message giving the tree after inserting the 
%-- tuple, the value belonging to the key given and a boolean noticing the user if the value
%-- given belongs or not to the new tree. 
%-----------------------------------------------------------------------------------------------
%-----------------------------------------------------------------------------------------------

-module(test8).
-export([main/4,new/0]).
-record(node,{key,
              val,
              smaller='nil',
              larger='nil'}).

main(T,Insert,SearchKey,ExistsValue) ->
	{K,V}=Insert,
	T2=insert(K,V,T),
	Node=search(SearchKey,T),
	{Res,Key,Value}=Node,
	Belongs=has_value(ExistsValue,T2),
	%io:format("T2: ~p~n Value for key ~p: ~p~n Belongs ~p to the tree?: ~p~n",[T2,SearchKey,Value,ExistsValue,Belongs]), 
	{T2,Value,Belongs}.

new() -> 'nil'.

insert(Key,Val,'nil') -> #node{key=Key,val=Val};
insert(NewKey,NewVal,Node) when NewKey == Node#node.key -> Node#node{key=NewVal},Node#node{val=NewVal};
insert(NewKey,NewVal,Node) when NewKey < Node#node.key -> 
	Node#node{val=NewKey},Node#node{smaller=insert(NewKey,NewVal,Node#node.smaller)};
insert(NewKey,NewVal,Node) when NewKey > Node#node.key -> 
	Node#node{larger=insert(NewKey,NewVal,Node#node.larger)}.

search(Key,'nil')-> {'nok',Key,'undefined'};
search(Key,Node) when Key == Node#node.key -> {'ok',Node#node.key,Node#node.val};
search(Key,Node) when Key < Node#node.key -> search(Key,Node#node.smaller);
search(Key,Node) -> search(Key,Node#node.larger).

has_value(Val, Tree) -> 
	try has_value_implem(Val,Tree) of
		false -> false
	catch
		true -> true
	end.

has_value_implem(_,'nil') -> false;
has_value_implem(Val,Node) when Val == Node#node.val -> throw(true);
has_value_implem(Val,Node) -> has_value_implem(Val,Node#node.smaller), 
							has_value_implem(Val,Node#node.larger).
