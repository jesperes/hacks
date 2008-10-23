-module(problem4).
-compile(export_all).
-define(MAX, 999).

is_palindrom(N) ->
    List = integer_to_list(N),
    List == lists:reverse(List).

palindrome(900, 900, MaxP) ->
    MaxP;
palindrome(900, Y, MaxP) ->
    palindrome(?MAX, Y-1, MaxP);
palindrome(X, Y, MaxP) ->
    case is_palindrom(X*Y) of
	true ->
	    palindrome(X - 1, Y, lists:max([X*Y, MaxP]));
	false ->
	    palindrome(X - 1, Y, MaxP)
    end.

%%% Look for numbers in the order
%%% 999*999
%%%
%%% 998*999
%%% 998*998
%%%
%%% 997*999
%%% 997*998
%%% 997*997

problem4() ->
    palindrome(999, 999, 1).
