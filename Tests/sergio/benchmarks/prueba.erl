-module(prueba).
-compile(export_all).

main(X) ->   
    case X of 
        {1,3} -> 9;                       
        {1,Z} = {Y,2} -> 1;
        {1,B} -> {slice,X}
    end.