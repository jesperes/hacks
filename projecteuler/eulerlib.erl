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

    
    
