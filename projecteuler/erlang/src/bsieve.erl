%% ---------------------------------------------------------------------
%% @author Richard Carlsson <richard@it.uu.se>
%% @copyright 2006 Richard Carlsson

-module(bsieve).

-export([main/0, main/1, sieve/1]).

-define(WORDSIZE, 27).
-define(TRUEBITS(N), ((1 bsl (N)) - 1)).

main() -> main(["2"]).

main([Arg]) ->
    N = list_to_integer(Arg),
    if N >= 2 ->
	    sieve((1 bsl N) * 10000),
	    sieve((1 bsl (N - 1)) * 10000),
	    sieve((1 bsl (N - 2)) * 10000);
       true ->
	    erlang:error(badarg)
    end.

sieve(M) ->
    Arr = make_array(M),
    Count = sieve(lookup_word(Arr, 0), 2, 0, 0, Arr, M div ?WORDSIZE),
    io:format("Primes up to ~8w~8w\n", [M, Count]),
    ets:delete(Arr),
    Count.
     
sieve(0, _Bit, Word, Count, Arr, LastWord) ->
    sieve_nextword(Word, LastWord, Arr, Count);
sieve(Curr, Bit, Word, Count, Arr, LastWord) ->
    if Bit < ?WORDSIZE ->
	    case Curr band (1 bsl Bit) of
		0 ->
		    sieve(Curr, Bit + 1, Word, Count, Arr, LastWord);
		_ ->
		    Curr1 = clear_bits(Curr, Bit, Word, LastWord, Arr),
		    sieve(Curr1, Bit + 1, Word, Count + 1,
			  Arr, LastWord)
	    end;
       true ->
	    sieve_nextword(Word, LastWord, Arr, Count)
    end.

sieve_nextword(Word, LastWord, Arr, Count) ->
    NextWord = Word + 1,
    if NextWord =< LastWord ->
	    Curr = lookup_word(Arr, NextWord),
	    sieve(Curr, 0, NextWord, Count, Arr, LastWord);
       true ->
	    Count
    end.


clear_bits(Curr, Bit, Word, LastWord, Arr) ->
    clear_next(Curr, Bit, Word, ?WORDSIZE * Word + Bit, LastWord, Arr),
    lookup_word(Arr, Word).

clear_bits(Curr, Bit, Word, Step, LastWord, Arr) ->
    clear_next(Curr band (bnot (1 bsl Bit)),
	       Bit, Word, Step, LastWord, Arr).

clear_next(Curr, Bit, Word, Step, LastWord, Arr) ->
    NextBit = Bit + Step,
    if NextBit < ?WORDSIZE ->
	    clear_bits(Curr, NextBit, Word, Step, LastWord, Arr);
       true ->
	    ets:insert(Arr, {Word, Curr}),
	    NextWord = Word + NextBit div ?WORDSIZE,
	    if NextWord =< LastWord ->
		    clear_bits(lookup_word(Arr, NextWord),
			       NextBit rem ?WORDSIZE, NextWord, Step,
			       LastWord, Arr);
	       true -> ok
	    end
    end.


lookup_word(Arr, Word) ->
    element(2,hd(ets:lookup(Arr, Word))).

make_array(M) ->
    T = ets:new(?MODULE, [ordered_set, private]),
    init_array(0, M div ?WORDSIZE, M rem ?WORDSIZE, T).

init_array(I, W, R, T) when I < W ->
    ets:insert(T, {I, ?TRUEBITS(?WORDSIZE)}),
    init_array(I + 1, W, R, T);
init_array(I, _, R, T) ->
    ets:insert(T, {I, ?TRUEBITS(R)}),
    T.
