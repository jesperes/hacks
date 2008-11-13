-module(problem7).
-export([problem7/1]).

problem7(Count) ->
    problem7(1, Count).

problem7(N, 0) ->
    N;
problem7(N, Count) ->
    Next = N + 2,
    IsPrime = eulerlib:is_prime(Next),
    if IsPrime ->
	    problem7(Next, Count - 1);
       true ->
	    problem7(Next, Count)
    end.
