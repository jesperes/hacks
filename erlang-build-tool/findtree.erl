-module(findtree).
-compile(export_all).
-import(filelib, [fold_files/5]).

% Return all files under Dir.
find(Dir) ->
    fold_files(Dir, ".*", true, fun(F,AccIn) -> [F|AccIn] end, []).


