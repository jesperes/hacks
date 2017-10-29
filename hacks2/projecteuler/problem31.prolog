currency(List) :-
	List = [N1,N2,N5,N10,N20,N50,N100,N200],
	200 #= N1 + N2 * 2 + N5 * 5 + N10 * 10 + N20 * 20 + N50 * 50 + N100 * 100 + N200 * 200,
	N1 #>= 0, N2 #>= 0, N5 #>= 0, N10 #>= 0,
	N20 #>= 0, N50 #>= 0, N100 #>= 0, N200 #>= 0,
	fd_labeling(List).

ways200(Number) :-
	findall(Denoms, currency(Denoms), X),
	length(X, Number).
