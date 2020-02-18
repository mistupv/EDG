-module(slicingExample2).
-export([main/0]).

main() ->
	{ _, N } = io:read(""),
	I = 1,
	
	Product = 1,
	{ _, NProduct } = while(I, N, undef, Product),
	
	io:write(NProduct).

while(I, N, _, Product) ->
	if I =< N ->
		
		NProduct = Product * I,
		NI = I + 1,
		while(NI, N, undef, NProduct);
	true ->
		{ undef, Product }
	end.