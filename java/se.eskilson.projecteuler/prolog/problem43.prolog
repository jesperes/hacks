problem43(X) :-
	X #=
	D1 * 1000000000 +
	D2 * 100000000 +
	D3 * 10000000 +
	D4 * 1000000 +
	D5 * 100000 +
	D6 * 10000 +
	D7 * 1000 +
	D8 * 100 +
	D9 * 10 +
	D10,

	D2 * 100 + D3 * 10 + D4 #= D234,
	D234 rem 2 #= 0,

	D3 * 100 + D4 * 10 + D5 #= D345,
	D345 rem 3 #= 0,

	D4 * 100 + D5 * 10 + D6 #= D456,
	D456 rem 5 #= 0,

	D5 * 100 + D6 * 10 + D7 #= D567,
	D567 rem 7 #= 0,

	D6 * 100 + D7 * 10 + D8 #= D678,
	D678 rem 11 #= 0,

	D7 * 100 + D8 * 10 + D9 #= D789,
	D789 rem 13 #= 0,

	D8 * 100 + D9 * 10 + D10 #= D8910,
	D8910 rem 17 #= 0,

	fd_labelling(X).


	
