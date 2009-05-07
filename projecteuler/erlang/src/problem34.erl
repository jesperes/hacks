-module(problem34).
-compile(export_all).

fac(N) when N =< 1 -> 1;
fac(N) -> N * fac(N-1).

facsum(N) ->
    lists:foldr(fun(X, Acc) ->
			fac(X) + Acc
		end, 0, eulerlib:digits(N)).

p34() ->
    p34(3, 0, 2000000).

p34(N, Sum, Limit) when N > Limit ->
    Sum;
p34(N, Sum, Limit) ->
    case (N rem 100000) of
	0 -> erlang:display({mark, N});
	_ -> true
    end,
    FacSum = facsum(N),
    case FacSum of
	N ->
	    erlang:display({facsum, N, Sum + N}),
	    p34(N+1, Sum + N, Limit);
	_ ->
	    p34(N+1, Sum, Limit)
    end.



