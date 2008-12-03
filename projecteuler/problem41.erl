-module(problem41).
-compile(export_all).

pd_prime() ->
    pd_prime(987654321).



pd_prime(N) ->
    
    eulerlib:is_prime(N)
