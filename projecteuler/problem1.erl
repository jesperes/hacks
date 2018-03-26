-module(problem1).
-compile(export_all).

% Is N a multiple of M
multiple_of(N, M) ->
   N rem M == 0.

problem1(0, X) ->
    X;
problem1(N, X) when N rem 3 == 0 ->
    problem1(N - 1, X + N);
problem1(N, X) when N rem 5 == 0 ->
    problem1(N - 1, X + N);
problem1(N, X) ->
    problem1(N - 1, X).

problem1(N) ->
    problem1(N-1, 0).
