-module(problem122).
-compile(export_all).

first_primefactor(F, N) when N rem F == 0 ->
    F;
first_primefactor(F, N) ->
    first_primefactor(F + 1, N).
    
power(Base, Exp) when Exp =< 1 ->
    {1, 0};
power(Base, Exp) when Exp rem 2 == 0 ->
    {X, N} = power(Base, Exp div 2),
    {X * X, N+1};
power(Base, Exp) ->
    FirstPrime = first_primefactor(2, Exp),
    if FirstPrime /= Exp ->
	    {F1, N1} = power(Base, FirstPrime),
	    {F2, N2} = power(Base, Exp - FirstPrime),
	    {F1*F2, N1+N2+1};
       true ->
	    {F, N} = power(Base, Exp-1),
	    {Base * F, N+1}
    end.


