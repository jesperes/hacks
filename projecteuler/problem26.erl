-module(problem26).
-compile(export_all).

%% For any prime number p > 5 the length of the repetend of 1/p is a
%% factor of p-1.

p26() ->
    List1 = lists:foldr(
	      fun(N, AccIn) ->    
		      Factors = eulerlib:primefactors(N),
		      case lists:filter(fun(X) -> (X /= 2) and (X /= 5)
					end, Factors) of
			  [] ->
			      AccIn;
			  NonBaseFactors ->
			      [{N, NonBaseFactors}|AccIn]
		      end
	      end,
	      [], lists:seq(2, 999)).
		  
		      
		      
			     
			 
