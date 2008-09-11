-module(ebt_server).
-compile(debug_info).
-export([start/0, start/1, register_client/1, request_file/2, build/0, log/1, log/2]).

-include_lib("kernel/include/file.hrl").

-record(state, {
	  srcdir,
	  files = [],
	  clients = [],
	  source_config_mod = false,
	  last_build = never,
	  pending_build = true
	  }).

%%% Public API
start() ->
    start(".").

monitor_config_file_loop(File, Parent) ->
    receive
	_ -> 
	    Parent ! config_file_changed,
	    monitor_config_file_loop(File, Parent)
    end.

monitor_config_file(File) ->
    log("Monitoring config file ~s for changes~n", [File]),
    Parent = self(),
    Pid = spawn(
	    fun() ->
		    monitor_config_file_loop(File, Parent)
	    end),
    file_monitor:monitor_file(File, Pid).

start([SrcDir|_]) ->
    crypto:start(),
    code:add_patha("/home/jesperes/eunit/ebin"),
    
    SrcDirAbs = filename:absname(SrcDir),
    file_monitor:start(),
    spawn(fun() ->
		  log("Started server: ~w~n", [self()]),
		  register(ebt_server, self()),
		  St = #state{ srcdir = SrcDirAbs },
		  St0 = load_source_config(St),
		  monitor_tree(self(), St0),
		  process_flag(trap_exit, true),
		  
		  ConfigFile = filename:absname_join(SrcDirAbs, "ebt_config.erl"),
		  monitor_config_file(ConfigFile),
		  
		  main(St0) 
	  end).

%% Register a client.
register_client(ServerHost) ->
    ServerNode = list_to_atom("ebt_server@" ++ ServerHost),
    erlang:send({ebt_server, ServerNode},
	        {register, self(), get_system_type()}),
    receive
	{ebt_server, ServerPid, NumFiles} ->
	    log("Registered with server ~w (tracking ~w files)~n",
		[ServerPid, NumFiles]),
	    {ServerPid, NumFiles}
    end.

request_file(ServerPid, F) ->
    ServerPid ! {get_file, self(), F}.

build() ->
    whereis(ebt_server) ! manual_build.

%%% INTERNAL
log(Str, Args) ->
    io:format("\r### Server> " ++ Str, Args).

log(Str) ->
    log(Str, []).

uname_m() ->
    S = os:cmd("uname -m"),
    list_to_atom(string:left(S, length(S) - 1)).

get_system_type() ->
    case os:type() of
	{win32, _OsName} ->
	    {win32, os:version()};
	{unix, OsName} ->
	    {OsName, uname_m()};
	X ->
	    log("Unknown system type: ~w~n", [X])
    end.

load_source_config(St) ->
    SrcFile = St#state.srcdir ++ "/ebt_config.erl",
    case c:c(SrcFile) of
	{ok, Module} ->
	    log("Reloaded source config.~n", []),
	    St#state{source_config_mod = Module,
		     pending_build = true};
	X ->
	    log("Failed to reload source config: ~w~n", [X]),
	    St
    end.


time_call(Fun, ResFun) ->
    statistics(wall_clock),
    Fun(),
    {_, Time} = statistics(wall_clock),
    ResFun(Time).

get_monitored_files(Dir, St) ->
    case St#state.source_config_mod of
	false ->
	    log("*** Failed to load source config module.~n"),
	    throw(error);
	SrcCfgMod ->
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
	    filelib:fold_files(".", ".*", true, F, {[], 0, 0})
    end.

monitor_file_or_dir(Node, Receiver) ->
    %% log("Monitoring: ~w~n", [Node]),
    IsDir = filelib:is_dir(Node),
    if IsDir ->
	    file_monitor:monitor_dir(Node, Receiver);
       true ->
	    file_monitor:monitor_file(Node, Receiver) 
    end.

monitor_tree(Receiver, St) ->
    time_call(
      fun() ->
	      log("Scanning directory for files to monitor: ~s~n", [St#state.srcdir]),
	      {Files, NumFiles, NumExcl} = get_monitored_files(St#state.srcdir, St),
	      log("Files to monitor: ~w (~w files excluded)~n", [NumFiles, NumExcl]),
	      lists:map(fun(F) -> monitor_file_or_dir(F, Receiver) end, Files)
      end,
      fun(Time) ->
	      log("Setup phase took ~g seconds.~n", [Time/1000])
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

output_filter(St, SystemType, String) ->
    Mod = St#state.source_config_mod,
    Mod:output_filter(SystemType, String).

get_build_message(St, SystemType, false) ->
    {St, {build, 
	  automatic_build(St, SystemType),
	  build_command(St, SystemType), 
	  build_dir(St, SystemType)}};

get_build_message(St, SystemType, true) ->
    {St, {build, 
	  true,
	  build_command(St, SystemType), 
	  build_dir(St, SystemType)}}.

broadcast_filechange([], _, _, _, _) -> true;
broadcast_filechange([{Client, _SystemType}|Clients], Path, Type, Fileinfo, St) ->
    case file:read_file(Path) of
	{ok, Binary} ->
	    Client ! {filechange, Path, Fileinfo, Binary};
	X ->
	    log("Failed to read file ~s: ~w~n", [Path, X])
    end,
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
    log("Sending file info to ~w (~w files)~n", [Client, length(St#state.files)]),
    lists:map(F, St#state.files).


add_client(Pid, SystemType, St) ->
    log("Registering client ~w (~w) at ~w~n", [Pid, SystemType, node(Pid)]),
    time_call(
      fun() ->
	      link(Pid),
	      Pid ! {ebt_server, self(), length(St#state.files)},
	      send_filelist_to_client(Pid, St),
	      Pid ! fileinfo_complete
      end,
      fun(Time) -> 
	      log("Sent file info to ~w (~g seconds)~n",
			[Pid, Time/1000])
      end),
    %% TODO: track pending_build state individually for each client
    St#state{ clients = [{Pid, SystemType}|St#state.clients],
	      pending_build = true }.


diff_time(A, B) ->
    {A1, A2, A3} = A,
    {B1, B2, B3} = B,
    {A1 - B1, A2 - B2, A3 - B3}.


trigger_build(St) ->
    trigger_build(St, false).

trigger_build(St, true) ->
    trigger_build(St#state.clients, St, true);
    
trigger_build(St, false) ->
    case St#state.last_build of
	never ->
	    trigger_build(St#state.clients, St, false);
	Time ->
	    Diff = diff_time(erlang:now(), Time),
	    if Diff > {0, 0, 5} ->
		    trigger_build(St#state.clients, St, false);
	       true ->
		    true
	    end
    end.

dispatch_build(St, Pid, SystemType, Force) ->
    case catch get_build_message(St, SystemType, Force) of
	{St0, {build, _, _, _} = Msg} ->
	    log("Triggering build on ~w (~w)~n", [Pid, SystemType]),
	    Pid ! Msg,
	    St0;
	false ->
	    false,
	    St;
	{'EXIT', {undef, [Fun|_]}} ->
	    log("Missing method in source config module: ~w~n", [Fun]),
	    St
    end.
    

trigger_build([], St, _Force) ->
    St;
trigger_build([{Pid, SystemType}|Clients], St, Force) ->
    St0 = dispatch_build(St, Pid, SystemType, Force),
    trigger_build(Clients, St0, Force).

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
	    log("No clients left.~n", []);
       true ->
	    log("Detected client exit. Remaining clients:~n", []),
	    lists:foreach(
	      fun(Client) ->
		      {Pid0, SystemType} = Client,
		      log("Client ~w (system type ~w)~n",
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
	    %% log("Sending file to client ~w (~w): ~w~n", [Client, node(Client), Path]),
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
	    %% ebt_config.erl is managed elsewhere.
	    case string:str(Path, "ebt_config.erl") of
		0 ->
		    log("Changed: ~s (~w, ~w bytes)~n", 
			[Path, 
			 FileInfo#file_info.mtime,
			 FileInfo#file_info.size]),
		    broadcast_filechange(St#state.clients, Path, Type, FileInfo, St),
		    main(St#state{
			   last_build = erlang:now(),
			   pending_build = true
			  });
		_ ->
		    main(St)
	    end;

	{file_monitor, _Ref, {error, Path, _Type, _Info}} ->
	    erlang:display({file_monitor, error, Path}),
	    main(St);

	%% Recevied when a client completes a build
	{build_complete, Pid, Status} ->
	    {_, SystemType} = get_client(Pid, St),
	    if Status == 0 ->
		    log("Build completed successfully at ~w by ~w (~w)~n", 
			      [time(), Pid, SystemType]);
	       true ->
		    log("Build failed on ~w (~w)~n", [Pid, SystemType])
	    end,
	    main(St);

	{'EXIT', Pid, _} ->
	    main(handle_client_exit(Pid, St));
	
	{build_output, Pid, String} ->
	    {_, SystemType} = get_client(Pid, St),
	    %% log(">>> ~p ~s~n", [SystemType, String]),
	    case catch output_filter(St, SystemType, String) of
		false ->
		    false;
		_ ->
		    io:format("   ~w ~s~n", [Pid, string:strip(String)])
	    end,
	    main(St);

	config_file_changed ->
	    log("Config file changed. Reloading.~n", []),
	    main(load_source_config(St));
	
	manual_build ->
	    main(St#state{pending_build = true});
	
	X ->
	    erlang:display(X),
	    main(St)
    
    after 1000 ->
	    if St#state.pending_build ->
		    St0 = trigger_build(St);
	       true ->
		    St0 = St
	    end,
	    main(St0#state{pending_build = false})
    end.
