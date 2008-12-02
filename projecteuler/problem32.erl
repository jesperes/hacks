-module(problem32).
-compile(export_all).

pandigital_check([], []) ->
    true;
pandigital_check([X|Xs], [Y|Ys]) when (X - $0) == Y ->
    pandigital_check(Xs, Ys);
pandigital_check(_, _) ->
    false.


%% Returns true if Numbers is a list of integers which taken together
%% are 1..N pandigital
pandigital(Numbers, N) ->
    List = lists:sort(
	     lists:concat(
	       lists:map(fun(X) -> integer_to_list(X) end, Numbers))),
    pandigital_check(List, lists:seq(1,N)).


nodups_check([]) ->
    true;
nodups_check([X,X|_]) ->
    false;
nodups_check([_|Xs]) ->
    nodups_check(Xs).

nodups(Numbers) ->
    List = lists:sort(
	     lists:concat(
	       lists:map(fun(X) -> integer_to_list(X) end, Numbers))),
    nodups_check(List).


get_pairs([]) ->
    [];
get_pairs([X|Xs]) ->
    [lists:foldr(fun(Y, AccIn) ->
			 case nodups([X, Y]) of
			     true ->
				 [{X,Y}|AccIn];
			     _ ->
				 AccIn
			 end
		 end, [], lists:seq(1, 999))|get_pairs(Xs)].

get_pairs() ->
    get_pairs(lists:seq(1, 9999)).

pandigital() ->
    statistics(runtime),
    io:format("Getting pairs...~n", []),
    Pairs = get_pairs(),
    Result = 
	lists:foldr(
	  fun({X, Y}, AccIn) ->
		  Z = X * Y,
		  case pandigital([X,Y,Z], 9) of
		      true ->
			  case lists:member(Z, AccIn) of
			      true ->
				  AccIn;
			      _ ->
				  [Z|AccIn]
			  end;
		      _ ->
			  AccIn
		  end
	  end, [], Pairs),
    {lists:sum(Result), {runtime, statistics(runtime)}}.


    





		   
