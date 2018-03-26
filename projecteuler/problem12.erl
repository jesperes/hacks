-module(problem12).
-compile(export_all).

-define(PRIMETABLE_LIMIT, 100000).

%% Call F with successive triangle numbers while F returns true.
%% Returns the last result of F (!= true).
map_triangle(F) ->
    map_triangle(1, 0, F).

map_triangle(N, Sum, F) ->
    case F(Sum + N) of
	true ->
	    map_triangle(N + 1, Sum + N, F);
	X ->
	    X
    end.

problem12(Limit) ->
    PrimeTable = primetable(),
    statistics(runtime),
    Result = map_triangle(
      fun(X) -> 
	      NumFactors = num_factors(X, PrimeTable),
	      if NumFactors > Limit ->
		      {X, NumFactors};
		 true ->
		      true
	      end
      end),
    {Result, statistics(runtime)}.


primetable() ->
    eulerlib:eratosthenes(?PRIMETABLE_LIMIT).

%% Calculate all prime factors of X.
prime_factors(_, [], Factors) ->
    Factors;
prime_factors(X, [Prime|Primes], Factors) when (X rem Prime) == 0 ->
    prime_factors(X div Prime, [Prime|Primes], [Prime|Factors]);
prime_factors(X, [_|Primes], Factors) ->
    prime_factors(X, Primes, Factors).

count_factors([], Acc) -> 
    Acc;
count_factors(PrimeFactors, Acc) ->
    [Head|_] = PrimeFactors,
    {NumFactors, Rest} = lists:partition(fun(X) -> X == Head end, PrimeFactors),
    count_factors(Rest, (length(NumFactors) + 1) * Acc).

num_factors(X, Primes) ->
    count_factors(prime_factors(X, Primes, []), 1).
