-module(prueba).
-export([numbers/2]).

main(X) ->   
    try                        
        X+4,
        A = f(5),
        X/0
    catch 
        5 -> X;
        error:Y -> {slice,X}
    end.

f(X) -> 
    throw(5),
    X+undef.

