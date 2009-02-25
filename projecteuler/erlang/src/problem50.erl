-module(problem50).
-compile(export_all).


p50(Primes) ->
    {List, _} = 
	lists:mapfoldl(fun(X, AccIn) ->
			       {{AccIn, X}, AccIn+1}
		       end, 1, Primes),
    Dict = dict:from_list(List),
    get_prime_sequence(Primes, Dict, length(Primes)-3).

%% Returns the longest prime sequence of length N.
get_prime_sequence(Primes, Dict, N) ->
    NPrimes = length(Primes),
    io:format("Checking sequences of size ~p~n", [N]),
    Sums = [ lists:sum(lists:sublist(Primes, Start, N)) || 
	       Start <- lists:seq(1, NPrimes - N + 1)],
    case lists:filter(fun(Sum) ->
			      dict:is_key(Sum, Dict)
		      end, Sums) of
	[] ->
	    get_prime_sequence(Primes, Dict, N-1);
	List ->
	    List
    end.



    


		      
