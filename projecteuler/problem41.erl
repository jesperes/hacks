-module(problem41).
-compile(export_all).


pandigital_prime() ->
    pandigital_prime(7654321).

pandigital_prime(0) ->
    false;
pandigital_prime(N) ->
    case eulerlib:pandigital(N) of
	true ->
	    case eulerlib:is_prime(N) of
		true ->
		    {pandigital_prime, N};
		_ ->
		    pandigital_prime(N-2)

	    end;
	_ ->
	    pandigital_prime(N-2)
    end.

	    
    
