triplet(Prod) :-
	X #\= 0, Y #\= 0, Z #\= 0,
	X**2 + Y**2 #= Z**2,
	X + Y + Z #= 1000,
	fd_labeling([X, Y, Z]),
	Prod is X * Y * Z.






