-module(palindrome).
-export([palindrome/1, palindrome_rev/1, test/1]).

palindrome(<<>>) ->
    true;
palindrome(<<_:8>>) ->
    true;
palindrome(Binary) when is_binary(Binary) ->
    Size = size(Binary)-2,
    <<First:8,Middle:Size/binary,Last:8>> = Binary,
    palindrome(First, Middle, Last).

palindrome(A, Middle, A) ->
    palindrome(Middle);
palindrome(_, _, _) ->
    false.

palindrome_rev(X) when is_binary(X) ->
    palindrome_rev(binary_to_list(X));
palindrome_rev(X) ->
    X == lists:reverse(X).

gen_palindrome(Chars) ->
    <<0:(Chars*8)>>.

test(Bytes) ->
    Binary = gen_palindrome(Bytes),
    statistics(runtime),
    Result = palindrome(Binary),
    {_, Time0} = statistics(runtime),
    io:format("palindrome(~p bytes) = ~p in ~p seconds~n",
	      [Bytes, Result, Time0/1000.0]),

    statistics(runtime),
    Result = palindrome_rev(Binary),
    {_, Time1} = statistics(runtime),
    io:format("palindrome_rev(~p bytes) = ~p in ~p seconds~n",
	      [Bytes, Result, Time1/1000.0]).
    

