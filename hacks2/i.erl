-module(i).
-compile(export_all).
l(K,[$||X])->l(K,X);
l(K,[K,$,,V|_])->V;
l(K,[_,$,,_|X])->l(K,X).
e(S,M)->lists:map(fun(X)->l(X,M)end,S).
