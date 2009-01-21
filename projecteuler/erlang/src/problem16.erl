-module(problem16).
-compile(export_all).

power(_, 0) -> 1;
power(Base, Exp) when Exp > 0 ->
    Base * power(Base, Exp - 1).

digits(N) when N < 10 ->
    [N];
digits(N) ->
    [N rem 10|digits(N div 10)].

p16() ->
    BigNum = power(2, 1000),
    Digits = digits(BigNum),
    lists:sum(Digits).
