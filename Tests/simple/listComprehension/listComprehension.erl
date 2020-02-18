-module(listComprehension).
-export([function/0]).

function() ->
	List = [ 1, 2, 3, 4, 5, 6 ],
	NList = [ Elem + 1 || Elem <- List ],
	3 * lists:sum(NList).