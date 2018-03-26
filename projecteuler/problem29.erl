-module(problem29).
-compile(export_all).

power(Base, Exp) ->
    eulerlib:power(Base, Exp).

p29(X,Y) ->
    length(lists:usort([power(A, B) || A <- lists:seq(X,Y), B <- lists:seq(X, Y)])).
	   

