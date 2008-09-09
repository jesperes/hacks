-module(ebt_client).
-export([start/1]).

-include_lib("kernel/include/file.hrl").

-record(state, {
	  files = [],
	  server, 
	  localcopy, 
	  fileinfo_count = 0, 
	  filecount = 0,
	  pending_build = false}).

start([Host, LocalCopy|_]) ->
    crypto:start(),

    io:format("Registering with server at ~s~n", [Host]),
    
    {ServerPid, NumFiles} = ebt_server:register_client(Host),

    io:format("Server pid is: ~p~n", [ServerPid]),
    io:format("Using local copy: ~p~n", [LocalCopy]),
    
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
		    %% io:format("Local copy is up-to-date: ~p~n", [LocalFile]),
		    file:write_file_info(LocalFile, FileInfo);
	       true ->
		    io:format("Local copy is not up-to-date (sha sum mismatch): ~p~n", [LocalFile]),
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
	    io:format(".");
	X ->
	    io:format("Failed to write file ~p: ~p~n", [X, LocalFile])
    end.

build_loop(Port, Parent) ->
    receive 
	{_, {exit_status, Exit}} ->
	    Parent ! {build_complete, Exit},
	    io:format("Build completed: ~p~n", [Exit]),
	    build_loop(Port, Parent);
	{_, {data, {eol, String}}} ->
	    Parent ! {child_output, String},
	    io:format("~s~n", [String]),
	    build_loop(Port, Parent);
	{'EXIT', _, Reason} ->
	    io:format("Build driver terminated: ~p~n", [Reason]);
	X ->
	    erlang:display({unknown, X}),
	    build_loop(Port, Parent)
    end.

open_build_port(Cmd, Dir) ->
    open_port({spawn, Cmd},
	      [{cd, Dir},
	       {line, 1024},
	       {env, [{"PACK5_NOPROGRESS", "true"},
		      {"PACK5_LOGLEVEL", "1"}]},
	       exit_status,
	       stderr_to_stdout]).

execute_build(_St, false, _Cmd, _Dir) ->
    io:format("Automatic build disabled.~n", []);

execute_build(St, true, Cmd, Dir) ->
    if St#state.pending_build ->
	    io:format("Build already pending, ignoring build request.~n", []),
	    St;
       true ->
	    Parent = self(),
	    spawn(fun() ->
			  AbsDir = filename:join(St#state.localcopy, Dir),
			  filelib:ensure_dir(AbsDir),
			  file:make_dir(AbsDir),
			  process_flag(trap_exit, true),
			  io:format("Building: ~s~n", [Cmd]),
			  Port = open_build_port(Cmd, AbsDir),
			  build_loop(Port, Parent)
		  end),
	    St#state{pending_build = true}
    end.

loop(St) ->
    receive
	fileinfo_complete ->
	    io:format("File info received (~p files)~n", [St#state.fileinfo_count]),
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
	    loop(St#state{pending_build = false});

	{child_output, String} ->
	    St#state.server ! {build_output, self(), String},
	    loop(St);

	{'EXIT', Pid, Reason} ->
	    if Pid == St#state.server ->
		    io:format("Build server exiting (~p). Exiting client.~n", [Reason]),
		    init:stop();
	       true ->
		    %% ignore other EXIT signals
		    loop(St)
	    end;
	X ->
	    erlang:display(X),
	    loop(St)
    after 2500 ->
	    %% io:format("Idle.~n", []),
	    loop(St)
    end.
	    
