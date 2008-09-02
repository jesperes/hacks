-module(ebt_server).
-compile(debug_info).
-export([start/0, start/1, register_client/1, request_file/2]).

-record(state, {
	  srcdir,
	  files = [],
	  clients = [],
	  source_config_file,
	  source_config_mod,
	  last_build = never,
	  pending_build = true
	  }).

%%% Public API
start() ->
    start(".").

start([SrcDir|_]) ->
    crypto:start(),
    code:add_patha("/home/jesperes/eunit/ebin"),
    
    SrcDirAbs = filename:absname(SrcDir),
    {SrcConfigMod, File} = load_source_config_module(SrcDirAbs),
    io:format("\rLoaded source config module: ~p~n", [File]),
    io:format("Exclude pattern: ~p~n", [SrcConfigMod:get_excludes()]),
    %% io:format("Autobuild: ~p~n", [SrcConfigMod:automatic_build()]),
    file_monitor:start(),
    spawn(fun() ->
		  register(ebt_server, self()),
		  St = #state{ 
		    source_config_mod = SrcConfigMod,
		    source_config_file = File,
		    srcdir = SrcDirAbs },
		  monitor_tree(self(), St),
		  process_flag(trap_exit, true),
		  main(St) 
	  end).

%% Register a client.
register_client(ServerHost) ->
    ServerNode = list_to_atom("ebt_server@" ++ ServerHost),
    erlang:send({ebt_server, ServerNode},
	        {register, self(), get_system_type()}),
    receive
	{ebt_server, ServerPid} ->
	    ServerPid
    end.

request_file(ServerPid, F) ->
    ServerPid ! {get_file, self(), F}.

%%% INTERNAL
uname_m() ->
    S = os:cmd("uname -m"),
    list_to_atom(string:left(S, length(S) - 1)).
    
get_system_type() ->
    case os:type() of
	{win32, OsName} ->
	    {OsName, os:version()};
	{unix, OsName} ->
	    {OsName, uname_m()};
	X ->
	    io:format("Unknown system type: ~p~n", [X])
    end.

get_source_file(SrcDir, File) ->
    filename:absname_join(SrcDir, File).

get_default_source_config_module() ->
    "source_config.erl".

load_default_source_config_module() ->
    File = filename:absname(get_default_source_config_module()),
    case c:c(File) of
	{ok, Module} ->
	    {Module, File};
	X ->
	    io:format("Failed to load (default) source module: ~p~n", [X]),
	    throw(load_source_config)
    end.

load_source_config_module(SrcDir) ->
    File = get_source_file(SrcDir, "ebt_config.erl"),
    case c:c(File, [{outdir, SrcDir}]) of
	{ok, Module} ->
	    {Module, File};
	_ ->
	    load_default_source_config_module()
    end.

reload_source_config(St) ->
    %% io:format("Reloading source config.~n", []),
    case c:c(St#state.source_config_file, {outdir, St#state.srcdir}) of
	{ok, _} ->
	    true;
	X ->
	    io:format("Failed to reload source config: ~p~n", [X])
    end.

time_call(Fun, ResFun) ->
    statistics(wall_clock),
    Fun(),
    {_, Time} = statistics(wall_clock),
    ResFun(Time).

get_monitored_files(Dir, St) ->
    SrcCfgMod = St#state.source_config_mod,
    {ok, ParsedRE} = regexp:parse(SrcCfgMod:get_excludes()),
    F = 
	fun(F, AccIn) ->
		{Files, Num, NumExcl} = AccIn,
		case regexp:matches(F, ParsedRE) of
		    {match, []} ->
			{[F|Files], Num + 1, NumExcl};
		    _ -> 
			{Files, Num, NumExcl + 1}
		end
	end,
    file:set_cwd(Dir),
    filelib:fold_files(".", ".*", true, F, {[], 0, 0}).

monitor_file_or_dir(Node, Receiver) ->
    %% io:format("Monitoring: ~p~n", [Node]),
    IsDir = filelib:is_dir(Node),
    if IsDir ->
	    file_monitor:monitor_dir(Node, Receiver);
       true ->
	    file_monitor:monitor_file(Node, Receiver) 
    end.

monitor_tree(Receiver, St) ->
    time_call(
      fun() ->
	      {ok, Cwd} = file:get_cwd(),
	      io:format("\rScanning directory for files to monitor: ~p~n", [Cwd]),
	      {Files, NumFiles, NumExcl} = get_monitored_files(St#state.srcdir, St),
	      io:format("\rFiles to monitor: ~p (~p files excluded)~n", [NumFiles, NumExcl]),
	      lists:map(fun(F) -> monitor_file_or_dir(F, Receiver) end, Files)
      end,
      fun(Time) ->
	      io:format("\rSetup phase took ~g seconds.~n", [Time/1000])
      end).

automatic_build(St, SystemType) ->
    Mod = St#state.source_config_mod,
    Mod:automatic_build(SystemType).

build_command(St, SystemType) ->
    Mod = St#state.source_config_mod,
    Mod:build_command(SystemType).

build_dir(St, SystemType) ->
    Mod = St#state.source_config_mod,
    Mod:build_dir(SystemType).

get_build_message(St, SystemType) ->
    reload_source_config(St),
    case automatic_build(St, SystemType) of
	true ->
	    {build, build_command(St, SystemType), build_dir(St, SystemType)};
	false ->
	    false
    end.

broadcast_filechange([], _, _, _, _) -> true;
broadcast_filechange([{Client, _SystemType}|Clients], Path, Type, Fileinfo, St) ->
    {ok, Binary} = file:read_file(Path),
    Client ! {filechange, Path, Fileinfo, Binary},
    broadcast_filechange(Clients, Path, Type, Fileinfo, St).

send_file_to_client([], _, _) ->
    true;
send_file_to_client([F|_], Client, F) ->
    {ok, Binary} = file:read_file(F),
    {ok, FileInfo} = file:read_file_info(F),
    Client ! {get_file, F, FileInfo, Binary};
send_file_to_client([_|Files], Client, F) ->
    send_file_to_client(Files, Client, F).


%% Send a list of file info + checksums to the client so the 
%% client then can decide which files it actually wants.
send_fileinfo_to_client(File, Client) ->
    {ok, FileInfo} = file:read_file_info(File),
    {ok, Binary} = file:read_file(File),
    Sha = crypto:sha(Binary),
    Client ! {fileinfo, File, FileInfo, Sha}.
send_filelist_to_client(Client, St) ->
    F = fun(File) ->
		send_fileinfo_to_client(File, Client)
	end,
    lists:map(F, St#state.files).


add_client(Pid, SystemType, St) ->
    io:format("Registering client ~p (~p) at ~p~n", [Pid, SystemType, node(Pid)]),
    time_call(
      fun() ->
	      link(Pid),
	      Pid ! {ebt_server, self()},
	      send_filelist_to_client(Pid, St),
	      Pid ! fileinfo_complete
      end,
      fun(Time) -> 
	      io:format("Sent file info to ~p (~g seconds)~n",
			[Pid, Time/1000])
      end),
    St#state{ clients = [{Pid, SystemType}|St#state.clients],
	      pending_build = true }.


diff_time(A, B) ->
    {A1, A2, A3} = A,
    {B1, B2, B3} = B,
    {A1 - B1, A2 - B2, A3 - B3}.

trigger_build(St) ->
    case St#state.last_build of
	never ->
	    trigger_build(St#state.clients, St);
	Time ->
	    Diff = diff_time(erlang:now(), Time),
	    if Diff > {0, 0, 5} ->
		    trigger_build(St#state.clients, St);
	       true ->
		    true
	    end
    end.


trigger_build([], _) ->
    true;
trigger_build([{Pid, SystemType}|Clients], St) ->
    io:format("Triggering build on ~p (~p)~n", [Pid, SystemType]),
    case catch get_build_message(St, SystemType) of
	{build, _, _} = Msg ->
	    Pid ! Msg;
	{'EXIT', {undef, [Fun|_]}} ->
	    io:format("Missing method in source config module: ~p~n", [Fun])
    end,
    trigger_build(Clients, St).


get_client(Pid, St) ->
    get_client0(St#state.clients, Pid).

get_client0([], _) ->
    false;
get_client0([{Pid, _} = Client|_], Pid) ->
    Client;
get_client0([_|Clients], Pid) ->
    get_client0(Clients, Pid).


remove_client(Pid, St) ->
    St#state{clients = 
	     lists:filter(
	       fun(Client) ->
		       {Pid0, _} = Client,
		       Pid0 /= Pid
	       end, St#state.clients)}.

handle_client_exit(Pid, St) ->
    NumClients = length(St#state.clients),
    St0 = remove_client(Pid, St),
    NumClients0 = length(St0#state.clients),
    
    if NumClients == NumClients0 ->
	    true;
       NumClients0 == 0 ->
	    io:format("No clients left.~n", []);
       true ->
	    io:format("Detected client exit. Remaining clients:~n", []),
	    lists:foreach(
	      fun(Client) ->
		      {Pid0, SystemType} = Client,
		      io:format("Client ~p (system type ~p)~n",
				[Pid0, SystemType])
	      end,
	      St0#state.clients)
    end,
    St0.

main(St) ->
    receive
	%% Register new client
	{register, Pid, SystemType} ->
	    main(add_client(Pid, SystemType, St));

	%% Request file contents
	{get_file, Client, Path} ->
	    %% io:format("Sending file to client ~p (~p): ~p~n", [Client, node(Client), Path]),
	    send_file_to_client(St#state.files, Client, Path),
	    main(St);

	%% Messages received from the file_monitor service
	{file_monitor, _Ref, {exists, Path, Type, FileInfo, _}} ->
	    Files = St#state.files,
	    broadcast_filechange(St#state.clients, Path, Type, FileInfo, St),
	    main(St#state{ 
		   files = [Path|Files],
		   last_build = erlang:now(),
		   pending_build = true
		  });

	{file_monitor, _Ref, {changed, Path, Type, FileInfo, _}} ->
	    io:format("Changed: ~w~n", [Path]),
	    broadcast_filechange(St#state.clients, Path, Type, FileInfo, St),
	    main(St#state{
		   last_build = erlang:now(),
		   pending_build = true
		  });

	{file_monitor, _Ref, {error, Path, _Type, _Info}} ->
	    erlang:display({file_monitor, error, Path}),
	    main(St);

	%% Recevied when a client completes a build
	{build_complete, Pid, Status} ->
	    {_, SystemType} = get_client(Pid, St),
	    if Status == 0 ->
		    io:format("Build completed successfully by ~p (~p)~n", [Pid, SystemType]);
	       true ->
		    io:format("Build failed on ~p (~p)~n", [Pid, SystemType])
	    end,
	    main(St);

	{'EXIT', Pid, _} ->
	    main(handle_client_exit(Pid, St));
	
	{build_output, Pid, String} ->
	    io:format("~w: ~s~n", [Pid, string:strip(String)]),
	    main(St);
	
	X ->
	    throw(X),
	    main(St)
    
    after 1000 ->
	    if St#state.pending_build ->
		    trigger_build(St);
	       true ->
		    true
	    end,
	    main(St#state{pending_build = false})
    end.