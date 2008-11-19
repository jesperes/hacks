-module(problem25).
-compile(export_all).

p25(Limit) ->
    fib({1,1,0}, Limit).
   
fib({N,X1,X2}, Limit) ->
    L = length(eulerlib:digits(X1)),
    if L >= Limit ->
	    N;
       true ->
	    fib({N+1, X1+X2, X1}, Limit)
    end.
 
