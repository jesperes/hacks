-module(findtree).
-compile(export_all).
-import(filelib, [fold_files/5]).

% Return all files under Dir.
find(Dir) ->
    fold_files(Dir, ".*", true, fun(F,AccIn) -> [F|AccIn] end, []).


find(Dir, Exclude) ->
    fold_files(Dir, ".*", true, 
	       fun(F,AccIn) -> 
		       case regexp:matches(F, Exclude) of
			   {match, [{_,_}]} ->
			       AccIn;
			   _ ->
			       [F|AccIn] 
		       end
	       end,
	       []).
