-module(ebt_client).
-export([start/1]).

-include_lib("kernel/include/file.hrl").

-record(state, {
	  files = [],
	  server, 
	  localcopy, 
	  fileinfo_count = 0, 
	  filecount = 0,
	  building = false,
	  pending_build = false,
	  build_command = {},
	  setup_phase = true}).

log(Str) ->
    log(Str, []).
log(Str, Args) ->
    io:format("\r### Client> " ++ Str, Args).

start([Host, LocalCopy|_]) ->
    crypto:start(),

    log("Registering with server at ~s~n", [Host]),
    
    {ServerPid, NumFiles} = ebt_server:register_client(Host),

    log("Server pid is: ~w~n", [ServerPid]),
    log("Using local copy: ~s~n", [LocalCopy]),
    
    filelib:ensure_dir(LocalCopy),

    St = #state{files = [], 
		server = ServerPid, 
		localcopy = LocalCopy,
		filecount = NumFiles},
    loop(St).

get_local_filename(File, St) ->
    filename:absname_join(St#state.localcopy, File).

is_local_file_uptodate(LocalFile, _LocalFileInfo, _RemoteFileInfo, RemoteSha) ->
    case file:read_file(LocalFile) of
	{ok, Binary} ->
	    LocalSha = crypto:sha(Binary),
	    LocalSha == RemoteSha;
	Reason ->
	    log("Failed to check local file: ~w~n", [Reason])
    end.



check_file({File, RemoteFileInfo, RemoteSha}, St) ->
    LocalFile = get_local_filename(File, St),
    case file:read_file_info(LocalFile) of
	{ok, LocalFileInfo} ->	    %% file exists
	    UpToDate = is_local_file_uptodate(LocalFile, LocalFileInfo, 
					      RemoteFileInfo, RemoteSha),
	    if UpToDate ->
		    true;
	       true ->
		    log("Requesting file: ~s~n", [LocalFile]),
		    ebt_server:request_file(St#state.server, File)
	    end;
	{error, enoent} ->
	    log("Requesting file (does not exist): ~s~n", [File]),
	    ebt_server:request_file(St#state.server, File);
	{error, X} ->
	    log("Unknown error: ~w (~s)~n", [X, File])
    end.

verify_mtime(Path, Mtime) ->
    {ok, Fi} = file:read_file_info(Path),
    case Fi#file_info.mtime of
	Mtime ->
	    true;
	X ->
	    throw({set_mtime_failed, Path, X, Mtime})
    end.

write_file(File, FileInfo, Binary, St) ->
    LocalFile = get_local_filename(File, St),
    ok = filelib:ensure_dir(filename:absname(LocalFile)),
    case file:write_file(LocalFile, Binary) of
	ok ->
	    Mtime = FileInfo#file_info.mtime,
	    ok = file:change_time(LocalFile, Mtime, Mtime),
	    verify_mtime(LocalFile, FileInfo#file_info.mtime),
	    log("Updated: ~s~n", [LocalFile]);
	X ->
	    log("Failed to write file ~w: ~s~n", [X, LocalFile])
    end.

build_loop(Port, Parent, Buf) ->
    receive 
	{_, {exit_status, Exit}} ->
	    Parent ! {build_complete, Exit},
	    log("Build completed: ~w~n", [Exit]),
	    build_loop(Port, Parent, Buf);
	{_, {data, {eol, String}}} ->
	    io:format("~s~n", [String]),
	    Str = Buf ++ String,
	    Parent ! {child_output, Str},
	    build_loop(Port, Parent, []);
	{_, {data, {noeol, String}}} ->
	    io:format("~s", [String]),
	    build_loop(Port, Parent, Buf ++ String);
	{'EXIT', _, Reason} ->
	    log("Build driver terminated: ~w~n", [Reason]);
	X ->
	    erlang:display({unknown, X}),
	    build_loop(Port, Parent, Buf)
    end.

open_build_port(Cmd, Dir) ->
    open_port({spawn, Cmd},
	      [{cd, Dir},
	       {line, 100},
	       {env, [{"PACK5_NOPROGRESS", "true"},
		      {"PACK5_LOGLEVEL", "1"},
		      {"LC_CTYPE", "C"}]},
	       exit_status,
	       hide,
	       stderr_to_stdout]).

execute_build(St, false, _Cmd, _Dir) ->
    log("Automatic build disabled.~n", []),
    St;

execute_build(St, true, Cmd, Dir) ->
    if St#state.building ->
	    log("Build in progress, queuing build request.~n", []),
	    St#state{pending_build = true,
		     build_command = {Cmd, true, Dir}};
       true ->
	    Parent = self(),
	    spawn(fun() ->
			  AbsDir = filename:join(St#state.localcopy, Dir),
			  filelib:ensure_dir(AbsDir),
			  file:make_dir(AbsDir),
			  process_flag(trap_exit, true),
			  log("************************************************************~n"),
			  log("************************************************************~n"),
			  log("************************************************************~n"),
			  log("Building: ~s~n", [Cmd]),
			  Port = open_build_port(Cmd, AbsDir),
			  build_loop(Port, Parent, [])
		  end),
	    St#state{build_command = {Cmd, true, Dir},
		     building = true,
		     pending_build = false}
    end.

loop(St) ->
    receive
	{initial_filelist, {Files, Dirs}} ->
	    log("Got initial file list: ~w files, ~w dirs~n", [length(Files),
							       length(Dirs)]),
	    log("Updating directory structure...~n"),
	    lists:map(fun(D) -> 
			      filelib:ensure_dir(get_local_filename(D, St)),
			      file:make_dir(get_local_filename(D, St))
		      end, Dirs),
	    log("Updating files...~n"),
	    lists:map(fun(F) -> check_file(F, St) end, Files),
	    loop(St);

	{file_changed, Path, FileInfo, Binary} ->
	    write_file(Path, FileInfo, Binary, St),
	    loop(St);

	{mkdir, Dir} ->
	    log("Creating directory: ~s~n", [Dir]),
	    LocalDir = get_local_filename(Dir, St),
	    filelib:ensure_dir(LocalDir),
	    file:make_dir(LocalDir),
	    loop(St);
	
	{delete, File} ->
	    LocalFile = get_local_filename(File, St),
	    log("Deleting ~s~n", [LocalFile]),
	    case filelib:is_dir(LocalFile) of
		true ->
		    case file:del_dir(LocalFile) of
			ok ->
			    log("Deleted directory: ~s~n", [File]),
			    loop(St);
			{error, Reason} ->
			    log("Failed to delete directory: ~s~n", [Reason]),
			    loop(St)
		    end;
		false ->
		    case file:delete(LocalFile) of
			ok ->
			    log("Deleted file: ~s~n", [File]),
			    loop(St);
			{error, enoent} ->
			    log("Ignoring delete_file request for nonexisting file: ~s~n",
				[File]),
			    loop(St);
			{error, Reason} ->
			    log("Failed to delete file: ~s~n", [Reason]),
			    loop(St)
		    end
	    end;

	{build, AutoBuild, BuildCmd, BuildDir} ->
	    loop(execute_build(St, AutoBuild, BuildCmd, BuildDir));
	
	{build_complete, Status} ->
	    St#state.server ! {build_complete, self(), Status},
	    {Cmd, AutoBuild, Dir} = St#state.build_command,
	    St_1 = St#state{building = false},
	    if St_1#state.pending_build ->
		    log("Executing queued build...~n", []),
		    loop(execute_build(St_1, AutoBuild, Cmd, Dir));
	       true ->
		    loop(St_1)
	    end;

	{child_output, String} ->
	    St#state.server ! {build_output, self(), String},
	    loop(St);
	
	{'EXIT', Pid, Reason} ->
	    if Pid == St#state.server ->
		    log("Build server exiting (~w). Exiting client.~n", [Reason]),
		    init:stop();
	       true ->
		    %% ignore other EXIT signals
		    loop(St)
	    end;
	X ->
	    erlang:display(X),
	    loop(St)
    after 2500 ->
	    if St#state.setup_phase ->
		    log("Setup phase complete.~n", []),
		    loop(St#state{setup_phase = false});
	       true ->
		    loop(St)
	    end
    end.

