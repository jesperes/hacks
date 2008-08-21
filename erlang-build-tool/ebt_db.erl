-module(ebt_db).
-export([start/0]).

-record(file, 
	{name, 				     %% filename
	 sha1sum,			     %% file checksum
	 last_checked,			     %% time of last check
	 mtime,				     %% last modification time of file
	 contents			     %% binary blob with file contents
	}).

-record(updates,
	{time,
	 what,
	 files_changed}).

default_table_defs() ->
    [{disc_only_copies, [node()]}].

tables() ->
    [file, updates].

table_def(file) ->
    [{attributes, record_info(fields, file)} |
     default_table_defs()];

table_def(updates) ->
    [{attributes, record_info(fields, updates)} | 
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
    spawn(fun() -> loop() end).

make_file_record(Path) ->
    {ok, Binary} = file:read_file(Path),
    #file{name = Path,
	  last_checked = now(),
	  mtime = filelib:last_modified(Path),
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
	    if Mtime > FileRecord#file.mtime ->
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
	X ->
	    erlang:display(X),
	    loop()
    end.
