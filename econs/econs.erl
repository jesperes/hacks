-module(econs).
-export([econs/1, econs/2]).

econs(BuildFile) ->
    Targets = build:add_toplevel(BuildFile),
    io:format("Targets: ~p~n", [Targets]).

econs(BuildFile, BuildDir) ->
    Targets = build:add_toplevel(BuildFile, BuildDir),
    io:format("Targets: ~p~n", [Targets]).



