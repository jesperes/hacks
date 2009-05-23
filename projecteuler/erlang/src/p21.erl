-module(p21).
-compile(export_all).


sum_divisors(N) ->
    lists:sum(divisors(N)).

divisors(N) ->
    divisors(N, 1, []).

divisors(N, N, Acc) -> Acc;
divisors(N, Factor, Acc) when N rem Factor == 0 ->
    divisors(N, Factor + 1, [Factor|Acc]);
divisors(N, Factor, Acc) ->
    divisors(N, Factor + 1, Acc).

is_amicable(N) ->
    sum_divisors(sum_divisors(N)) == N.
    
sum_amicable(Limit) ->    
    lists:foldr(fun(X, AccIn) ->
			IsAmicable = is_amicable(X),
			if IsAmicable ->
				X + AccIn;
			   true ->
				AccIn
			end
		end, 0, lists:seq(1, Limit-1)).
    
    

			
