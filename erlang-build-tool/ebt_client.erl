-module(ebt_client).
-export([start/1]).
-include("ebt_db.hrl").

-record(state, {}).

get_server_node(Host) ->
    list_to_atom("ebt_server@" ++ Host).

get_server_pid(Host) ->
    erlang:send({ebt_server, get_server_node(Host)}, {getpid, self()}),
    receive 
	{pid, ServerPid} ->
	    ServerPid
    end.

start_db() ->
    mnesia:start(),
    mnesia:subscribe(system),
    mnesia:subscribe({table, file, simple}),
    mnesia:set_debug_level(debug).

start([Host, LocalCopy|_]) ->
    io:format("Waiting for server at ~p~n", [get_server_node(Host)]),
    ServerPid = get_server_pid(Host),
    io:format("Server pid is: ~p~n", [ServerPid]),

    case mnesia:create_schema([node()]) of
	ok ->
	    io:format("Created new database for local copy.~n"),
	    start_db();
	{error, {Db, {already_exists, _}}} ->
	    io:format("Using existing local copy db: ~p~n", [Db]),
	    start_db();
	Error ->
	    throw(Error)
    end,
    
    io:format("Using local copy: ~p~n", [LocalCopy]),
    
    ServerPid ! {add_table_copy, node(), self()},
    file:set_cwd(LocalCopy),
    loop(#state{}).

compare_file_info(F1, F2) ->
    if ((F1#file_info.size == F2#file_info.size) and
	(F1#file_info.mtime == F2#file_info.mtime)) ->
	    true;
       true ->
	    false
    end.

%% Return true/false if the clients local copy is up-to-date.
is_local_copy_up_to_date(Record) ->
    case file:read_file_info(Record#file.name) of
	{ok, LocalFileInfo} ->
	    compare_file_info(Record#file.file_info, LocalFileInfo);
	{error, Reason} ->
	    erlang:display({is_local_copy_up_to_date, Reason}),
	    false
    end.

write_local_file(Record) ->
    FileInfo = Record#file.file_info,
    case file:open(Record#file.name, [write, raw, binary]) of
	{ok, IoDevice} ->
	    file:write(IoDevice, Record#file.contents),
	    %% TODO: update permissions, etc.
	    io:format("Updated ~p (~p bytes).~n", [Record#file.name, FileInfo#file_info.size]);
	{error, Reason} ->
	    erlang:display({write_local_file, Reason})
    end.

update_local_file(Record) ->
    case is_local_copy_up_to_date(Record) of
	true ->
	    [];
	_ ->
	    write_local_file(Record)
    end.

%% Update this client's local copy.
update_local_copy() ->
    Fun = fun(Record, AccIn) ->
		  io:format("Updating local file: ~p~n", [Record#file.name]),
		  update_local_file(Record),
		  AccIn
	  end,
    mnesia:transaction(
      fun() -> mnesia:foldl(Fun, [], file) end).

loop(St) ->
    receive
	added_table_copy ->
	    io:format("Table added.~n", []),
	    update_local_copy(),
	    io:format("Local copy updated.~n", []),
	    loop(St);
	X ->
	    {unknown, self(), X},
	    loop(St)
    end.


    

