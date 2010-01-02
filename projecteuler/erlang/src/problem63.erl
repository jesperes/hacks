-module(problem63).
-compile(export_all).

num_digits(N, Exp) ->
    length(eulerlib:num_digits(eulerlib:power(N, Exp))).

p63(Exp) ->
    p63(Exp, 0).

p63(Exp, Base) ->
    p63(Exp, Base, 1).
p63(Exp, Base, Acc) ->
    NumD = num_digits(Base, Exp),
    if
	NumD < Exp ->
	    p63(Exp, Base+1, Acc);
	NumD > Exp ->
	    Acc;
	true ->
	    erlang:display({match, Base, Exp, eulerlib:power(Base, Exp)}),
	    p63(Exp, Base+1, Acc+1)
    end.


p63_2(N) ->
    p63_2(N, 0).

p63_2(1, Acc) ->
    Acc;
p63_2(Exp, Acc) ->
    p63_2(Exp-1, p63(Exp) + Acc).




