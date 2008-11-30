perimeter([P,X,Y,Z]) :-
	X #>= 1,
	Y #> X,
	Z #> Y,
	P #= X + Y + Z,
	X ** 2 + Y ** 2 #= Z ** 2,
	fd_labeling([P,X,Y,Z]).

p39(0, X, X).
p39(P, [MaxP, MaxLen], Out) :-
	findall([X,Y,Z], perimeter([P,X,Y,Z]), Solutions),
	length(Solutions, NewLen),
	P0 is P - 1,
	(NewLen > MaxLen -> 
	    p39(P0, [P, NewLen], Out);
	    p39(P0, [MaxP, MaxLen], Out)).

p39(Max) :-
	p39(1000, [0, 0], Max).
