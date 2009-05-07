-module(problem33).
-compile(export_all).

itol(N) ->
    lists:map(fun(X) -> X - $0 end, integer_to_list(N)).

p33() ->
    List = [ {Nom, itol(Nom),
	      Denom, itol(Denom)} || Nom <- lists:seq(10, 99),
				     Denom <- lists:seq(10, 99),
				     Nom /= Denom,
				     Nom < Denom,
				     Nom rem 10 /= 0,
				     Denom rem 10 /= 0],
    
    Fractions = 
	lists:foldr(fun({Nom, [N,N1], Denom, [N,D1]}, Xs) when Nom/Denom == N1/D1 ->
			    [{Nom, Denom, N1, D1}|Xs];
		       ({Nom, [N1,N], Denom, [D1,N]}, Xs) when Nom/Denom == N1/D1 ->
			    [{Nom, Denom, N1, D1}|Xs];
		       ({Nom, [N,N1], Denom, [D1,N]}, Xs) when Nom/Denom == N1/D1 ->
			    [{Nom, Denom, N1, D1}|Xs];
		       ({Nom, [N1,N], Denom, [N,D1]}, Xs) when Nom/Denom == N1/D1 ->
			    [{Nom, Denom, N1, D1}|Xs];
		       (X, Xs) ->
			    Xs
		    end, [], List),
    
    {Nom, Denom} = 
	lists:foldr(fun({Nom, Denom, _, _}, {AccNom, AccDenom}) ->
			    {AccNom * Nom, AccDenom * Denom}
		    end, {1, 1}, Fractions),
    {Nom, Denom}.





    
    

	    
