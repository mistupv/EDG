-module(slicingExample).
-export([main/0]).

main() ->
	{ ok, N } = io:read(""),
	I = 1,
	Sum = 0,
	Product = 1,
	{ NSum, NProduct } = while(I, N, Sum, Product),
	io:write(NSum),
	io:write(NProduct).

while(I, N, Sum, Product) ->
	if I =< N ->
		NSum = Sum + I,
		NProduct = Product * I,
		NI = I + 1,
		while(NI, N, NSum, NProduct);
	true ->
		{ Sum, Product }
	end.