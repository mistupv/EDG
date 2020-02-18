-module(prueba).
-export([numbers/2]).

main(X) ->   
    try                        
        X+4,
        f(5),
        X/0
    catch 
        5 -> 2;
        error:_ -> {slice,X}
    end.

f(X) -> 
    X-1,
    X/2,
    X+undef.

g(X) ->
    f(X).