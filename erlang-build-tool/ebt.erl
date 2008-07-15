-module(ebt).
-export([main/0]).
-compile(export_all).
-import(filelib, [fold_files/5]).
-include(ebt_record).
-include_lib("stdlib/include/qlc.hrl").

skip_file(F0 ++ ".svn" ++ F1) ->
    true.
skip_file(F0 ++ ".git" ++ F1) ->
    true.
skip_file(_) ->
    false.

tree_visitor(F, TreeName, _Acc) ->
    case skip_file(
	{match, [{_,_}]} ->
	    [];
	_ ->
	    Row = #file{name=F, treename=TreeName, sha1sum="xxx"},
	    mnesia:write(Row),
	    []
    end.

load_tree(Dir, TreeName) ->
    io:format("Loading ~p into database...~n", [Dir]),
    F = fun() ->
		fold_files(Dir, ".*", true, 
			   fun(F, Acc) -> 
				   tree_visitor(F, TreeName, Acc) 
			   end, [])
	end,
    mnesia:transaction(F),
    io:format("Done.~n").

db_info() ->
    F = fun() ->
		X0 = qlc:eval(qlc:q([X#file.name || X <- mnesia:table(file)])),
		io:format("~p~n", [X0])
	end,
    mnesia:transaction(F).

create_files_table() ->
    Attrs = {attributes, record_info(fields, file)},
    case mnesia:create_table(file, [Attrs, {disc_copies, [node()]}]) of
	{atomic, ok} ->
	    [];
	Error ->
	    throw(Error)
    end.

init_mnesia() ->
    case mnesia:create_schema([node()]) of
	ok ->
	    io:format("Created new Mnesia schema.~n"),
	    mnesia:start(),
	    create_files_table();
of
	{error, {Db, {already_exists, _}}} ->
	    io:format("Using Mnesia schema: ~p~n", [Db]),
	    mnesia:start();

	Error ->
	    throw(Error)
    end.

main() ->
    init_mnesia(),
    Args = init:get_arguments(),
    %% io:format("args: ~p~n", [Args]),
    X = parse_args(Args),
    io:format("Result: ~p~n", [X]),
    mnesia:stop(),
    init:stop().
