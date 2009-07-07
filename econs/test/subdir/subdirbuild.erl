-module(subdirbuild).
-export([build/1, configure/1]).
-behaviour(build_script).


configure(_) ->
    true.

build(Env) ->
    io:format("building here!: ~p~n", [Env]),
    true.


