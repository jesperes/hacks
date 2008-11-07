-module(eulerlib).
-compile(export_all).

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



eratosthenes(Limit) ->
    CrossLimit = floor(math:sqrt(Limit)),
    Sieve = initial_sieve(Limit),
    lists:filter(fun(notprime) ->
			 false;
		    (N) ->
			 true
		 end,
		 array:to_list(array:map(
				 fun
				     (I, true) -> 
					 notprime ;
				     (I, false) -> 
					 I 
				 end,
				 sieve(3, Sieve, CrossLimit)))).

%% Initial sieve; mark all even numbers. (Numbers marked as true
%% are NOT primes.
initial_sieve(Limit) ->
    array:map(
      fun(N, _) -> 
	      if N < 2 ->
		      true;
		 N == 2 ->
		      false;
		 (N > 2) and ((N rem 2) == 0) ->
		      true;
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
