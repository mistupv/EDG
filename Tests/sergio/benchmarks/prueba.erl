-module(prueba).
-compile(export_all).

main(X) ->   
    case f(X) of 
        {1,3} -> 9;                       
        {1,B} -> {slice,X}
    end.
f(X) -> 
    case {X,4} of
        {1,4} -> {1,X};
        _ -> {X,12}
    end.