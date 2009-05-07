-module(testbuild).
-export([build/1, configure/1]).

-behavior(build_script).

configure(_) ->
    true.

build(Env) ->
    io:format("building here!: ~p~n", [Env]),
    build:add_subdirectory(Env, "subdir/subdirbuild.erl"),
    node:create_new(Env, file, false, builder).
