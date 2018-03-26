-module(eulerlib).
-compile(export_all).

power(_,0) -> 1;
power(1, _) -> 1;
power(Base, Exp) when (Exp rem 2) == 0 ->
    HalfBase = power(Base, Exp div 2),
    HalfBase * HalfBase;
power(Base, Exp) ->
    Base * power(Base, Exp-1).


floor(X) ->
    T = trunc(X),
    case X - T < 0 of
        true -> T - 1;
        false -> T
    end.

ceiling(X) ->
    T = trunc(X),
    case X - T < 0 of
        true -> T;
        false -> T + 1
    end.

is_prime(1) ->
    false;
is_prime(N) when N < 4 ->
    true;
is_prime(N) when (N rem 2) == 0 ->
    false;
is_prime(N) when N < 9 ->
    %% 4, 6, and 8 are already excluded.
    true;
is_prime(N) when (N rem 3) == 0 ->
    false;
%% All primes greater than 3 can be written as 6K +/- 1.
%% Any number n can have only one primefactor greater than sqrt(n).
is_prime(N) ->
    R = floor(math:sqrt(N)),
    F = 5,
    is_prime(N, F, R).

is_prime(_, F, R) when F > R ->
    true;
is_prime(N, F, _) when N rem F == 0 ->
    false;
is_prime(N, F, _) when N rem (F+2) == 0 ->
    false;
is_prime(N, F, R) ->
    is_prime(N, F + 6, R).



%% Sieve of Erastosthenes
eratosthenes(Limit) ->
    Sieve = sieve(3, initial_sieve(Limit), floor(math:sqrt(Limit))),
    array:foldr(fun(_, true, AccIn) ->
			AccIn;
		   (I, false, AccIn) ->
			[I|AccIn]
		end, [], Sieve).

%% Initial sieve; mark all even numbers. (Numbers marked as true
%% are NOT primes.
initial_sieve(Limit) ->
    array:map(
      fun(N, _) ->
	      if (N > 2) and ((N rem 2) == 0) ->
		      true;
		 N < 2 ->
		      true;
		 N == 2 ->
		      false;
		 true ->
		      false
	      end
      end,
      array:new(Limit+1)).

crossout(M, Sieve, Step) ->
    Size = array:size(Sieve),
    if M >= Size ->
	    Sieve;
       true ->
	    crossout(M + Step, array:set(M, true, Sieve), Step)
    end.

sieve(N, Sieve, CrossLimit) when N > CrossLimit ->
    Sieve;
sieve(N, Sieve, CrossLimit) ->
    Next = N + 2,
    case array:get(N, Sieve) of
	%% N not marked, hence prime. Cross out multiples
	%% of N, starting at N*N.
	false ->
	    sieve(Next, crossout(N*N, Sieve, 2*N), CrossLimit);

	%% N is marked, hence not prime. Continue with next.
	%%
	_ ->
	    sieve(Next, Sieve, CrossLimit)
    end.


%% Brute force version of finding the number of divisors.
num_factors_bf(X) ->
    num_factors_bf(X, 1, 0, math:sqrt(X)).
num_factors_bf(_, N, NumF, Sqrt) when N > Sqrt ->
    2 * NumF;
num_factors_bf(_, N, NumF, Sqrt) when N == Sqrt ->
    2 * NumF + 1;
num_factors_bf(X, N, NumF, Sqrt) when (X rem N) == 0 ->
    num_factors_bf(X, N + 1, NumF + 1, Sqrt);
num_factors_bf(X, N, NumF, Sqrt) ->
    num_factors_bf(X, N + 1, NumF, Sqrt).


%% Returns the digits in N
digits(N) ->
    lists:reverse(digits0(N)).
digits0(N) when N < 10 ->
    [N];
digits0(N) ->
    [N rem 10|digits0(N div 10)].

%% Factorial function
fac(0) -> 1;
fac(N) when N > 0 ->
    erlang:display({fac, N}),
    fac(N-1) * N.

comb(N,R) when R < N ->
    erlang:display({comb, N, R}),
    fac(N) div (fac(R) * fac(N-R)).


fib(N) ->
    {X, _} = fib0(N),
    X.
fib0(1) -> {1, 0};
fib0(2) -> {1, 1};
fib0(N) ->
    {X1,X2} = fib0(N-1),
    {X1+X2, X1}.

%% Returns a list of primefactors of N
primefactors(N) ->
    Factors = primefactors(2, N, [], N),
    %% erlang:display(lists:foldr(fun(X,Acc) -> X*Acc end, 1, Factors)),
    lists:reverse(Factors).

primefactors(F, _N, Acc, Limit) when F > Limit ->
    Acc;
primefactors(F, N, Acc, Limit) when N rem F > 0 ->
    primefactors(F + 1, N, Acc, Limit);
primefactors(F, N, Acc, Limit) when N rem F == 0 ->
    primefactors(F, N div F, [F|Acc], Limit).


perms([]) ->
    [[]];
perms(L) ->
    [[H|T] || H <- L, T <- perms(L--[H])].


pandigital(P) ->
    PList = integer_to_list(P),
    N = length(PList),
    CountArray =
	lists:foldr(
	  fun(X, Acc) ->
		  case Acc of
		      false ->
			  false;
		      Array ->
			  C = X - $0 - 1,
			  ValidIndex = (C >= 0) and (C < N),
			  if ValidIndex ->
				  case array:get(C, Array) of
				      undefined ->
					  array:set(C, set, Array);
				      _ ->
					  false
				  end;
			     true ->
				  false
			  end
		  end
	  end,
	  array:new(N), PList),
    case CountArray of
	false ->
	    false;
	Array ->
	    array:foldr(fun(_, undefined, _) ->
				false;
			   (_, _, Acc) ->
				Acc
			end, true, Array)
    end.


foldint(F, Acc, From, To) ->
    lists:reverse(foldint0(F, Acc, From, To)).

foldint0(F, InitalAcc, From, To) when From < To ->
    NextAcc = F(From, InitalAcc),
    foldint0(F, NextAcc, From+1, To);
foldint0(F, InitalAcc, From, From) ->
    F(From, InitalAcc);
foldint0(_, Acc, From, To) when From > To ->
    Acc.





rev_digit_sum(N) ->
    N + list_to_integer(lists:map(fun(X) -> X + $0 end, 
				  lists:reverse(digits(N)))).

is_palindrom(N) ->
    List = integer_to_list(N),
    List == lists:reverse(List).

is_lychrel(N) ->
    is_lychrel(N, 1).
is_lychrel(N, Iter) when Iter < 50 ->
    RevSum = rev_digit_sum(N),
    %% erlang:display({revsum, RevSum}),
    case is_palindrom(RevSum) of
	true ->
	    {Iter, N, RevSum};
	false ->
	    is_lychrel(RevSum, Iter+1)
    end;
is_lychrel(_, _) ->
    true.


lychrel() ->
    lychrel(9999, []).

lychrel(9,Acc) ->
    Acc;
lychrel(N,Acc) ->
    case is_lychrel(N) of
	true ->
	    %% erlang:display({lychrel, N}),
	    lychrel(N-1, [N|Acc]);
	_ ->
	    lychrel(N-1, Acc)
    end.





			     
    
