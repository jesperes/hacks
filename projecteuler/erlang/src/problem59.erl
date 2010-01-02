-module(problem59).
-compile(export_all).

min(A,B) when A < B -> A;
min(_,B) -> B.

tokenize(File) ->
    {ok, Bin} = file:read_file(File),
    lists:map(fun(X) ->
		      list_to_integer(X)
	      end,
	      string:tokens(binary_to_list(Bin), ",\r\n")).

decrypt(ClearTxt, Key) ->
    lists:reverse(decrypt(ClearTxt, Key, Key, [])).
decrypt([], _, _, Acc) ->
    Acc;
decrypt(ClearTxt, [], Key, Acc) ->
    %% Restart with Key
    decrypt(ClearTxt, Key, Key, Acc);
decrypt([C|ClearTxt], [K|Key], OrigKey, Acc) ->
    decrypt(ClearTxt, Key, OrigKey, [C bxor K | Acc]).

verify([]) ->
    true;
verify([C|Rest]) when (C >= 65) and (C =< 90) ->
    verify(Rest);
verify([_C|_Rest]) ->
    false.

decrypt_file(File, Key) ->
    decrypt(tokenize(File), Key).

decrypt_and_check(File, Key) ->
    Result = decrypt_file(File, Key),
    io:format("Result = ~p~n", [Result]),
    verify(Result).





