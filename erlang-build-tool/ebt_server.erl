-module(ebt_server).
-compile(export_all).
-include("ebt_record.hrl").
-include_lib("stdlib/include/qlc.hrl").

default_table_defs() ->
    [{ram_copies, [node()]}].

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


    
%% Create the file table.
create_tables() ->
    lists:map(fun create_table/1, tables()).

%% Removes all file records from the database.
purge() ->
    F = fun() ->
		mnesia:clear_table(file),
		mnesia:write(#updates{time=now(), what=purge, files_changed=all})
	end,
    mnesia:transaction(F).

show_updates() ->
    F = fun() ->
		qlc:eval(qlc:q([X || X <- mnesia:table(updates)]))
	end,
    mnesia:transaction(F).


%% Open/create file database
open_db() ->
    mnesia:stop(),
    case mnesia:create_schema([node()]) of
	ok ->
	    io:format("Created new Mnesia schema.~n"),
	    mnesia:start(),
	    create_tables();
	
	{error, {Db, {already_exists, _}}} ->
	    io:format("Using Mnesia schema: ~p~n", [Db]),
	    mnesia:start();

	Error ->
	    throw(Error)
    end.

start(_SrcDir, _Name) ->
    open_db().
    

stop() ->
    mnesia:dump_tables(tables()).
    
