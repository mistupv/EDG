-module(prueba).
-compile(export_all).

main() ->
    X = true,
    try 
        B = (catch f(1)),
        {slice,B}
    catch true -> 3			
    end.

f(X) ->
	case X of
		1 ->
			throw(true);
		_ -> 1
	end.