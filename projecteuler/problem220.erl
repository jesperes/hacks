-module(problem220).
-compile(export_all).

turn_left({0, -1}) -> {1, 0};
turn_left({1, 0}) -> {0, 1};
turn_left({0, 1}) -> {-1, 0};
turn_left({-1, 0}) -> {0, -1}.

turn_right({0, -1}) -> {-1, 0};
turn_right({-1, 0}) -> {0, 1};
turn_right({0, 1}) -> {1, 0};
turn_right({1, 0}) -> {0, -1}.


direction(N) ->
    case (((N band -N) bsl 1) band N) of
	0 ->
	    left;
	1 ->
	    right
    end.


