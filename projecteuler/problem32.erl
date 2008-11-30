-module(problem32).
-compile(export_all).

-define(LIMIT, 999).

gen_prods(0) ->
    [];
gen_prods(N) ->
    erlang:display({gen_prods, N}),
    case nodups(N) of
	true ->
	    [gen_prods(?LIMIT, N)|gen_prods(N-1)];
	_ ->
	    gen_prods(N-1)
    end.

gen_prods(0, _) ->
    [];
en_prods(M, N) ->
    erlang:display({gen_prods, N, M}),
    case nodups([M, N]) of
	true ->
	    [gen_prods(?LIMIT, M, N)|gen_prods(M-1, N)];
	_ ->
	    gen_prods(M-1, N)
    end.


gen_prods(0, _, _) ->
    [];
gen_prods(P, M, N) ->
    erlang:display({gen_prods, N, M, P}),
    case nodups([P, M, N]) of
	true ->
	    [{N, M, P}|gen_prods(P-1, M, N)];
	_ ->
	    gen_prods(P-1, M, N)
    end.

nodups(N) when is_integer(N) ->
    nodups0(lists:sort(integer_to_list(N)));
nodups(List) when is_list(List) ->
    nodups0(lists:sort(
	      lists:concat(
		lists:map(fun(X) ->
				  integer_to_list(X)
			  end, List)))).
nodups0([]) ->
    true;
nodups0([X,X|_]) ->
    false;
nodups0([_|Xs]) ->
    nodups0(Xs).


   
