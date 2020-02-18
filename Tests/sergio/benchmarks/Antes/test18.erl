-module(test18).
-export([main/2]).
%INPUTS: X puede ser terminate, tupla de 2 elementos o cualquier otra cosa. Y puede ser cualquier cosa (num,lista,tupla), siempre se ignora
main(X,Y) ->
	Z = case X of
		terminate -> "the end";
		{A,B} -> {[A+B,B-A],3};
		{3,C} -> g(C);
		_ -> {20*3,8}
	end,
	T = 2,
	V = f(T)+h(2)+h(3), 
	W = g([X,Y,{X,Y}]), 
	Tuple = {Z,W,V}, 
	Tuple.

g(X) ->
	[_,_,{R,S}] = X,
	case R of
		[1,3] -> 21; % Solo cuando X = [1,3]
		[A,B] -> (A*B)/9;
		T ->  T;
		_ -> f(4)
	end. 

f(7) -> 
	L = 2+9,
	F = L*3,
	F+L;
f(4) -> 9;
f(2) -> 7;
f(X) -> X.

h(X) -> 
	case X of
		2-> j({2,4});
		3->	k([4,8]);
		1-> l(107)
	end.

j(A) -> {X,_} = A, X.
k(B) -> [H|T] = B, H.
l(C) -> C-1.