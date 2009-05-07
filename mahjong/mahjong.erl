-module(mahjong).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

% Ett parti mahjong består av fyra ronder, en för varje vind.

% Östan är den första rondens vind, ända tills varje spelare har varit
% östan.

% Antalet omgångar i ett helt parti är således minst 16 st.

%% Beräknar poängen för en given hand (dvs, par/pong/kong).
%% hand_points(PlayerWind, RoundWind, Hand)
hand_points(_, _, {pong, dragons, _, hidden}) -> 8;
hand_points(_, _, {pong, dragons, _, open}) -> 4;
hand_points(_, _, {pong, winds, _, hidden}) -> 8;
hand_points(_, _, {pong, winds, _, open}) -> 4;
hand_points(_, _, {pong, _, 1, hidden}) -> 8;
hand_points(_, _, {pong, _, 9, open}) -> 4;
hand_points(_, _, {pong, _, _, hidden}) -> 4;
hand_points(_, _, {pong, _, _, open}) -> 2;
hand_points(_, _, {kong, dragons, _, hidden}) -> 32;
hand_points(_, _, {kong, dragons, _, open}) -> 16;
hand_points(_, _, {kong, winds, _, hidden}) -> 32;
hand_points(_, _, {kong, winds, _, open}) -> 16;
hand_points(_, _, {kong, _, 1, hidden}) -> 32;
hand_points(_, _, {kong, _, 9, open}) -> 16;
hand_points(_, _, {kong, _, _, hidden}) -> 16;
hand_points(_, _, {kong, _, _, open}) -> 8;
hand_points(_, _, {pair, dragons, _, _}) -> 2;
hand_points(PlayerWind, _, {pair, winds, PlayerWind, _}) -> 2;
hand_points(_, RoundWind, {pair, winds, RoundWind, _}) -> 2;
hand_points(Wind, Wind, {pair, winds, Wind, _}) -> 4;
%% All other hands give 0 points.
hand_points(_, _, _) -> 0.

player_round_result(PlayerWind, RoundWind, Hands) ->
    lists:sum(lists:map(fun(Hand) ->
				hand_points(PlayerWind, RoundWind, Hand)
			end, Hands)).

player_round_result_test() ->
    PlayerWind = east,
    RoundWind = east,
    Hands = [{pong, dragons, green, hidden},
	     {kong, bamboo, 8, open}],
    ?assert(32 == player_round_result(PlayerWind, RoundWind, Hands)).

