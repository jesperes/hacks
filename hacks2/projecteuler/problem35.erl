-module(problem35).
-compile(export_all).

rotate(N) when N < 10 -> [N];
rotate(N) ->
    NStr = integer_to_list(N),
    {_, Rotates} = 
	lists:foldr(fun(_, {Num, Acc}) ->
			    Rot = rotate_int(Num),
			    {Rot, [Rot|Acc]}
		    end,
		    {NStr, [NStr]}, 
		    lists:seq(2,length(NStr))),
    lists:map(fun(X) ->
		      {Int, _} = string:to_integer(X),
		      Int
	      end,
	      Rotates).

rotate_int([]) -> [];
rotate_int([X|Xs]) ->
    Xs ++ [X].

all_prime(Primes) ->
    CircularPrimes = 
	lists:filter(
	  fun(Prime) ->
		  Rs = rotate(Prime),
		  case lists:all(fun(X) -> lists:member(X, Primes) end, Rs) of
		      true ->	
			  io:format("Circular: ~p (~p)~n", [Prime, Rs]),
			  true;
		      false ->
			  false
		  end
	  end, Primes),
    lists:foreach(fun erlang:display/1, CircularPrimes),
    {CircularPrimes, length(CircularPrimes)}.
