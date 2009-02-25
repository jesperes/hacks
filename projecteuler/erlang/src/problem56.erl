-module(problem56).
-compile(export_all).

power(_,0) -> 1;
power(1, _) -> 1;
power(Base, Exp) when (Exp rem 2) == 0 ->
    HalfBase = power(Base, Exp div 2),
    HalfBase * HalfBase;
power(Base, Exp) ->
    Base * power(Base, Exp-1).

digits(N) when N < 10 ->
    [N];
digits(N) ->
    [N rem 10|digits(N div 10)].

p56() ->
    lists:max([lists:sum(digits(power(A, B))) || A <- lists:seq(1, 100),
						 B <- lists:seq(1, 100) ]).


