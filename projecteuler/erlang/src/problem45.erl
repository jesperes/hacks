-module(problem45).
-compile(export_all).


triangle(X) ->
    X * (X+1) div 2.

pentagon(X) ->
    X * (3*X-1) div 2.

hexagon(X) ->
    X * (2*X-1).
        
%% Returns true if Ax^2 + Bx + C == 0 has integer solutions.
has_integer_solutions(A, B, C) ->
    Discr = (B * B) - 4*(A * C),
    SqrtDiscr = trunc(math:sqrt(Discr)),
    if SqrtDiscr * SqrtDiscr == Discr ->
	    %% Discriminant is a perfect square
	    Sol1F = (-B + SqrtDiscr) / (2*A),
	    Sol1I = trunc(Sol1F),
	    if Sol1I == Sol1F ->
		    Sol1I;
	       true ->
		    false
	    end;
       true ->
	    false
    end.

find_tph_equals(Hn) ->    
    N = hexagon(Hn),
    case has_integer_solutions(3, -1, -2*N) of
	false ->
	    %% No matching pentagonal number
	    find_tph_equals(Hn + 1);
	Pn ->
	    case has_integer_solutions(1, 1, -2*N) of
		false ->
		    find_tph_equals(Hn + 1);
		Tn ->
		    {equals, Tn, Pn, Hn, N}
	    end
    end.
