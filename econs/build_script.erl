-module(build_script).
-export([build/1, configure/1, behaviour_info/1]).

behaviour_info(callbacks) ->
    [{configure, 1},
     {build, 1}].

%% Called to setup the build rules for the directory.  Returns a
%% dependency graph node (or list of nodes) representing what to build
%% for this directory.
build(_) ->
    [].

configure(_) ->
    false.




