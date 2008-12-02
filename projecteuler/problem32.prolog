
pandigital(X, Y, Z) :-
	X #= X1 * 10 + X2,
	Y #= Y1 * 100 + Y2 * 10 + Y3,
	Z #= Z1 * 1000 + Z2 * 100 + Z3 * 10 + Z4,
	X * Y #= Z,
	all_different([X1, X2, Y1, Y2, Y3, Z1, Z2, Z3, Z4]),
	fd_labeling([X, Y, Z]).

