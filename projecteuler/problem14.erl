-module(problem14).
-compile(export_all).

%% Find start number < Limit yielding the longest iterative sequence.
%% Brute force solution. Optimization: cache sequence lengths for
%% starting numbers, and when you get a cache hit, you known the
%% length of that sequence.
seq(N) ->
    seq(N, 0).
seq(1, Acc) ->
    1 + Acc;
seq(N, Acc) when (N rem 2) == 0 ->
    seq(N div 2, 1 + Acc);
seq(N, Acc) ->
    seq(3*N + 1, 1 + Acc).

problem14(Limit) ->
    problem14(1, Limit, {0, 0}).
problem14(N, Limit, MaxChain) when N >= Limit ->
    MaxChain;
problem14(N, Limit, {_, MaxLen} = OldMax) ->
    Length = seq(N),
    if Length > MaxLen ->
	    problem14(N+1, Limit, {N, Length});
       true ->
	    problem14(N+1, Limit, OldMax)
    end.
