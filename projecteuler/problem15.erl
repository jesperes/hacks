-module(problem15).
-compile(export_all).

fac(1) -> 1;
fac(N) when N > 1 ->
    fac(N-1) * N.

