-module(p48).
-compile(export_all).

p48(1, Acc) ->
    1 + Acc;
p48(N, Acc) ->
    eulerlib:power(N, N) + p48(N-1, Acc).



    
