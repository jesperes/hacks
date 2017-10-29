-module(problem53).
-compile(export_all).

fac(0) -> 1;
fac(N) when N > 0 ->
    fac(N-1) * N.

comb(N,R) when R =< N ->
    fac(N) div (fac(R) * fac(N-R)).

p53() ->
    lists:filter(fun(X) -> X > 1000000 end,
		 [ comb(N,R) || N <- lists:seq(1, 100),
				R <- lists:seq(1, 100),
				N >= R ]).

    




    
