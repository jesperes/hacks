-module(problem10).
-compile(export_all).

num_primes(Limit) ->
    Primes = eulerlib:eratosthenes(Limit),
    length(Primes).

nth_prime(N, Max) ->
    lists:nth(N, eulerlib:eratosthenes(Max)).

sum_primes(Limit) ->
    lists:sum(eulerlib:eratosthenes(Limit)).
