-module(ebt_client).
-export([start/1]).

-include_lib("kernel/include/file.hrl").

-record(state, {files = [], server, localcopy, fileinfo_count = 0}).

start([Host, LocalCopy|_]) ->
    crypto:start(),

    io:format("Registering with server at ~s~n", [Host]),
    
    ServerPid = ebt_server:register_client(Host),

    io:format("Server pid is: ~p~n", [ServerPid]),
    io:format("Using local copy: ~p~n", [LocalCopy]),
    
    St = #state{files = [], server = ServerPid, localcopy = LocalCopy},
    loop(St).

get_local_filename(File, St) ->
    filename:absname_join(St#state.localcopy, File).

check_file(File, FileInfo, Sha, St) ->
    LocalFile = get_local_filename(File, St),
    case file:read_file_info(LocalFile) of
	{ok, _LocalFileInfo} ->	    %% file exists
	    {ok, Binary} = file:read_file(LocalFile),
	    LocalSha = crypto:sha(Binary),
	    if LocalSha == Sha ->
		    %% io:format("Local copy is up-to-date: ~p~n", [LocalFile]),
		    file:write_file_info(LocalFile, FileInfo);
	       true ->
		    io:format("Local copy is not up-to-date: ~p~n", [LocalFile]),
		    ebt_server:request_file(St#state.server, File)
	    end;
	{error, enoent} ->
	    io:format("File does not exist: ~p~n", [File]),
	    ebt_server:request_file(St#state.server, File);
	{error, X} ->
	    io:format("Unknown error: ~p (~p)~n", [X, File])
    end.
	    
write_file(File, FileInfo, Binary, St) ->
    LocalFile = get_local_filename(File, St),
    ok = filelib:ensure_dir(filename:absname(LocalFile)),
    case file:write_file(LocalFile, Binary) of
	ok ->
	    ok = file:write_file_info(LocalFile, FileInfo),
	    io:format("Updated local file: ~p~n", [LocalFile]);
	X ->
	    io:format("Failed to write file ~p: ~p~n", [X, LocalFile])
    end.

loop(St) ->
    receive
	fileinfo_complete ->
	    io:format("File info received (~p files)~n", [St#state.fileinfo_count]),
	    loop(St);
	{filechange, File, Fileinfo, Binary} ->
	    %%write_file(File, Fileinfo, Binary, St),
	    loop(St);
	{fileinfo, File, FileInfo, Sha} ->
	    check_file(File, FileInfo, Sha, St),
	    loop(St#state{fileinfo_count = St#state.fileinfo_count+1});
	{get_file, File, FileInfo, Binary} ->
	    %%write_file(File, FileInfo, Binary, St),
	    loop(St);
	X ->
	    erlang:display(X),
	    loop(St)
    after 2500 ->
	    %% io:format("Idle.~n", []),
	    loop(St)
    end.
	    
