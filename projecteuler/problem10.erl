-module(problem10).
-export([sieve/1, num_primes/1, nth_prime/2, sum_primes/1]).

sieve(Max) ->
    List = lists:filter(fun(X) -> 
				(X == 2) or ((X > 2) and ((X rem 2) /= 0))
			end,
			lists:seq(1, Max)), 
    lists:sort(sieve(List, math:sqrt(Max))).
sieve(List, SqrtMax) ->
    sieve(List, SqrtMax, []).

sieve([], _, Primes) ->
    Primes;
sieve([Prime|List], SqrtMax, Primes) when Prime >= SqrtMax ->
    lists:append([[Prime|List], Primes]);
sieve([Prime|List], SqrtMax, Primes) ->
    sieve(lists:filter(
	    fun(X) -> 
		    (X > Prime) and ((X rem Prime) /= 0) 
	    end, 
	    List), SqrtMax, [Prime|Primes]).

num_primes(Limit) ->
    Primes = sieve(Limit),
    length(Primes).

nth_prime(N, Max) ->
    lists:nth(N, sieve(Max)).

sum_primes(Limit) ->
    lists:sum(sieve(Limit)).
