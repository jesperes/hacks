-module(ebt_db).
-export([start/0]).

-include("ebt_db.hrl").
 
default_table_defs() ->
    [{disc_only_copies, [node()]}].

tables() ->
    [file].

table_def(file) ->
    [{attributes, record_info(fields, file)} |
     default_table_defs()].

create_table(TableName) ->
    Result = mnesia:create_table(TableName, table_def(TableName)),
    case Result of
	{atomic, ok} ->
	    TableName;
	Error ->
	    throw(Error)
    end.

start() ->
    mnesia:set_debug_level(debug),
    mnesia:stop(),
    crypto:start(),
    case mnesia:create_schema([node()]) of
	ok ->
	    io:format("Created new database.~n"),
	    mnesia:start(),
	    lists:map(fun create_table/1, tables());
	{error, {Db, {already_exists, _}}} ->
	    io:format("Using existing db: ~p~n", [Db]),
	    mnesia:start();
	Error ->
	    throw(Error)
    end,

    mnesia:subscribe(system),
    mnesia:subscribe({table, file, simple}),

    spawn(fun() -> loop() end).

make_file_record(Path) ->
    {ok, Binary} = file:read_file(Path),
    #file{name = Path,
	  file_info = file:read_file_info(Path),
	  contents = Binary}.
    
write_file_record(Path) ->
    mnesia:write(make_file_record(Path)).

update_file_trans(Path) ->
    case mnesia:read({file, Path}) of
	[] ->
	    io:format("File missing from db, adding: ~p~n", [Path]),
	    write_file_record(Path);
	[FileRecord] ->
	    Mtime = filelib:last_modified(Path),
	    FileInfo = FileRecord#file.file_info,
	    if Mtime > FileInfo#file_info.mtime ->
		    %% File has been modified
		    io:format("File has changed on disk, updating: ~p (~p bytes)~n", 
			      [Path, filelib:file_size(Path)]),
		    write_file_record(Path);
	       true ->
		    %% File is unchanged
		    %% io:format("File is unchanged: ~p~n", [Path])
		    []
	    end
    end.

update_file(Path) ->
    mnesia:transaction(
      fun() -> 
	      update_file_trans(Path)
      end).


loop() ->
    receive
	{exists, Path} ->
	    update_file(Path),
	    loop();
	{changed, Path} ->
	    update_file(Path),
	    loop();
	{add_table_copy, Node, From} ->
	    case mnesia:add_table_copy(file, Node, disc_only_copies) of
		{atomic, ok} ->
		    io:format("Added table copy on node ~p~n", [Node]),
		    From ! added_table_copy;
		Error ->
		    erlang:display({add_table_copy, failed, Error}),
		    From ! failed_add_table_copy
	    end,
	    loop();
	X ->
	    erlang:display({unknown, X}),
	    loop()
    end.
