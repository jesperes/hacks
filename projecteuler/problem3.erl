-module(problem3).
-compile(export_all).

-define(NUMBER, 600851475143).

problem3() ->
    problem3(2).

problem3(N) when N == ?NUMBER ->
    0;
problem3(N) when N > 2, N rem 2 == 0 ->
    problem3(N + 1);
problem3(N) when N > 3, N rem 3 == 0 ->
    problem3(N + 1);
problem3(N) when N > 5, N rem 5 == 0 ->
    problem3(N + 1);
problem3(N) when ?NUMBER rem N == 0 ->
    io:format("Factor: ~p~n", [N]),
    problem3(N + 1);
problem3(N) ->
    problem3(N + 1).



