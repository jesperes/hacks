-module(tool).
-export([create_builder/2]).

create_builder(c_shared_obj, Env) -> 
    builder:create_builder(Env, ["action to build shared c object"]).

create_builder(shared_lib, Env) -> 
    builder:create_builder(Env, ["action to build shared library"]).
