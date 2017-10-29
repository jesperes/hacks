-module(bil).
-compile(export_all).

%% Fuel consumption (l/km)
consumption() ->
    0.07.

%% Fuel cost (SEK/l)
fuelcost() ->
    14.

%% Cost (Dist == km)
cost(Dist) ->
    Dist * consumption() * fuelcost().
