-module(ebt_db).
-compile(export_all).

-record(file, 
	{name, 				     %% relative name within tree
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


remove_filename_prefix(Prefix, Filename) ->
    string:substr(Filename, length(Prefix)+2).

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


loop() ->
    receive
	X ->
	    erlang:display(X),
	    loop()
    end.
