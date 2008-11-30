p45([T,TN,PN]) :-
	T #>= 40755,
	TN #>= 285,
	PN #>= 165,
	%% HN #>= 143,
	TN #> PN,
	%% PN #> HN,
	T #= TN * (TN + 1)/2,
	T #= PN * (3 * PN - 1)/2,
	%% T #= HN * (2 * HN - 1),
	fd_labeling([T, TN, PN]).
