-module(problem2).
-compile(export_all).

problem2(_, X, N) when X > 4000000 ->
    N;
problem2(X, Y, N) when Y rem 2 == 0 ->
    problem2(Y, X+Y, N+Y);
problem2(X, Y, N) ->
    problem2(Y, X+Y, N).

problem2() ->
    problem2(1, 2, 0).

