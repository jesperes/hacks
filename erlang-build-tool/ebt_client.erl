-module(ebt_client).
-export([start/1]).

-include_lib("kernel/include/file.hrl").

-record(state, {
	  files = [],
	  server, 
	  localcopy, 
	  fileinfo_count = 0, 
	  filecount = 0,
	  pending_builds = 0,
	  pending_build_command = {},
	  setup_phase = true}).

log(Str, Args) ->
    io:format("\rClient ### " ++ Str, Args).

start([Host, LocalCopy|_]) ->
    crypto:start(),

    log("Registering with server at ~s~n", [Host]),
    
    {ServerPid, NumFiles} = ebt_server:register_client(Host),

    log("Server pid is: ~p~n", [ServerPid]),
    log("Using local copy: ~p~n", [LocalCopy]),
    
    filelib:ensure_dir(LocalCopy),

    St = #state{files = [], 
		server = ServerPid, 
		localcopy = LocalCopy,
		filecount = NumFiles},
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
		    %% log("Local copy is up-to-date: ~p~n", [LocalFile]),
		    file:write_file_info(LocalFile, FileInfo);
	       true ->
		    log("Requesting file (checksum): ~p~n", [LocalFile]),
		    ebt_server:request_file(St#state.server, File)
	    end;
	{error, enoent} ->
	    log("Requesting file (does not exist): ~p~n", [File]),
	    ebt_server:request_file(St#state.server, File);
	{error, X} ->
	    log("Unknown error: ~p (~p)~n", [X, File])
    end.
	    
write_file(File, FileInfo, Binary, St) ->
    LocalFile = get_local_filename(File, St),
    ok = filelib:ensure_dir(filename:absname(LocalFile)),
    case file:write_file(LocalFile, Binary) of
	ok ->
	    ok = file:write_file_info(LocalFile, FileInfo),
	    log("Updated: ~p~n", [LocalFile]);
	X ->
	    log("Failed to write file ~p: ~p~n", [X, LocalFile])
    end.

build_loop(Port, Parent, Buf) ->
    receive 
	{_, {exit_status, Exit}} ->
	    Parent ! {build_complete, Exit},
	    log("Build completed: ~p~n", [Exit]),
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
	    log("Build driver terminated: ~p~n", [Reason]);
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
    if St#state.pending_builds > 0 ->
	    log("Build already pending, queuing build request.~n", []),
	    St#state{pending_builds = St#state.pending_builds + 1,
		     pending_build_command = {Cmd, Dir}};
       true ->
	    Parent = self(),
	    spawn(fun() ->
			  AbsDir = filename:join(St#state.localcopy, Dir),
			  filelib:ensure_dir(AbsDir),
			  file:make_dir(AbsDir),
			  process_flag(trap_exit, true),
			  log("Building: ~s~n", [Cmd]),
			  Port = open_build_port(Cmd, AbsDir),
			  build_loop(Port, Parent, [])
		  end),
	    St#state{pending_builds = 0,
		     pending_build_command = {Cmd, Dir}}
    end.

execute_queued_builds(St) ->
    {Cmd, Dir} = St#state.pending_build_command,
    execute_build(St, true, Cmd, Dir).

loop(St) ->
    receive
	fileinfo_complete ->
	    log("File info received (~p files)~n", [St#state.fileinfo_count]),
	    loop(St);

	{filechange, File, Fileinfo, Binary} ->
	    write_file(File, Fileinfo, Binary, St),
	    loop(St);

	{fileinfo, File, FileInfo, Sha} ->
	    check_file(File, FileInfo, Sha, St),
	    loop(St#state{fileinfo_count = St#state.fileinfo_count+1});

	{get_file, File, FileInfo, Binary} ->
	    write_file(File, FileInfo, Binary, St),
	    loop(St);

	{build, AutoBuild, BuildCmd, BuildDir} ->
	    loop(execute_build(St, AutoBuild, BuildCmd, BuildDir));

	{build_complete, Status} ->
	    St#state.server ! {build_complete, self(), Status},
	    if St#state.pending_builds > 0 ->
		    loop(execute_queued_builds(St));
	       true ->
		    loop(St)
	    end;

	{child_output, String} ->
	    St#state.server ! {build_output, self(), String},
	    loop(St);

	{'EXIT', Pid, Reason} ->
	    if Pid == St#state.server ->
		    log("Build server exiting (~p). Exiting client.~n", [Reason]),
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
		    log("Setup phase complete. Listening for changes..~n", []),
		    loop(St#state{setup_phase = false});
	       true ->
		    loop(St)
	    end
    end.

