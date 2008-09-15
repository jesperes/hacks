-module(ebt_server).
-compile(debug_info).
-export([start/0, start/1, register_client/1, request_file/2, build/0, log/1, log/2]).

-include_lib("kernel/include/file.hrl").

-record(state, {
	  srcdir,
	  files = [],
	  dirs = [],
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
		  St0 = #state{ srcdir = SrcDirAbs },
		  St = load_source_config(St0),
		  
		  %% Monitor the root directory
		  file:set_cwd(SrcDirAbs),
		  file_monitor:monitor_dir(".", self()),
		  
		  process_flag(trap_exit, true),
		  ConfigFile = filename:absname_join(SrcDirAbs, "ebt_config.erl"),
		  monitor_config_file(ConfigFile),
		  main(St) 
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
    ServerPid ! {send_file, self(), F}.

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
    Value = Fun(),
    {_, Time} = statistics(wall_clock),
    ResFun(Time),
    Value.

%%% Returns true/false if File should be monitored
file_monitor_filter(St, File) ->
    case St#state.source_config_mod of
	false ->
	    true;
	SrcCfgMod ->
	    {ok, ParsedRE} = regexp:parse(SrcCfgMod:get_excludes()),
	    case regexp:matches(File, ParsedRE) of
		{match, []} ->
		    true;
		_ ->
		    false
	    end
    end.

%% monitor_file(File, Receiver, DictIn) ->
%%     Dir = filename:dirname(File),
%%     DictOut = 
%% 	case dict:is_key(Dir, DictIn) of
%% 	    false ->
%% 		log("Monitoring directory: ~s~n", [Dir]),
%% 		file_monitor:monitor_dir(Dir, Receiver),
%% 		dict:append(Dir, true, DictIn);
%% 	    _ ->
%% 		DictIn
%% 	end,
    
%%     log("Monitoring: ~s~n", [File]),
%%     file_monitor:monitor_file(File, Receiver),
%%     DictOut.


%% monitor_tree(Receiver, St) ->
%%     time_call(
%%       fun() ->
%% 	      log("Scanning directory for files to monitor: ~s~n", [St#state.srcdir]),
%% 	      {Files, NumFiles, NumExcl} = get_monitored_files(St#state.srcdir, St),
%% 	      log("Files to monitor: ~w (~w files excluded)~n", [NumFiles, NumExcl]),

%% 	      lists:foldl(fun(F, DictIn) -> 
%% 				  monitor_file(F, Receiver, DictIn)
%% 			  end, dict:new(), Files)
%%       end,
%%       fun(Time) ->
%% 	      log("Setup phase took ~g seconds.~n", [Time/1000])
%%       end).

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

get_initial_filelist(St) ->
    {lists:map(fun(File) ->
		       {ok, FileInfo} = file:read_file_info(File),
		       {ok, Binary} = file:read_file(File),
		       {File, FileInfo, crypto:sha(Binary)}
	       end, St#state.files),
     St#state.dirs}.

add_client(Pid, SystemType, St) ->
    log("Registering client ~w (~w) at ~w~n", [Pid, SystemType, node(Pid)]),
    link(Pid),
    Pid ! {ebt_server, self(), length(St#state.files)},
    log("Sending initial filelist to ~p~n", [Pid]),
    Pid ! {initial_filelist, get_initial_filelist(St)},
    log("Done.~n"),
    
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

monitor_file_or_dir(St, File, Receiver) ->
    case {file_monitor_filter(St, File), filelib:is_dir(File)} of
	{false, _} ->
	    false;
	{true, true} ->
	    log("Monitoring (dir): ~s~n", [File]),
	    file_monitor:monitor_dir(File, Receiver);
	{true, false} ->
	    log("Monitoring (file): ~s~n", [File]),
	    file_monitor:monitor_file(File, Receiver)
    end.

%% Add monitors for all entries in a directory
monitor_dir_entry(St, Path, F) ->
    case F of
	{added, File} ->
	    SubPath = filename:join(Path, File),
	    monitor_file_or_dir(St, SubPath, self());
	X ->
	    log("Unknown: ~w~n", [X])
    end.

add_file(St, Path, AddMonitor) ->
    case file_monitor_filter(St, Path) of
	true ->
	    case AddMonitor of
		true ->
		    monitor_file_or_dir(St, Path, self()),
		    multicast_file(St, Path);
		false ->
		    []
	    end,
	    St#state{files = [Path|St#state.files]};
	false ->
	    log("*** Warning: added monitor for excluded file!~n", []),
	    St
    end.


%%% When an entire file-tree is removed (or renamed), we are not
%%% guaranteed that events are generated such that the directories can
%%% be removed bottom-up.

handle_file_changes(St, _Ref, {exists, Path, file, _FileInfo, []}) ->
    add_file(St, Path, false);

handle_file_changes(St, _Ref, {exists, Path, dir, _FileInfo, Files}) ->
    lists:map(fun(F) -> monitor_dir_entry(St, Path, F) end, Files),
    St#state{dirs = [Path|St#state.dirs]};

handle_file_changes(St, _Ref, {changed, Path, file, _FileInfo, Files}) ->
    log("Changed (file): ~s, ~w~n", [Path, Files]),
    multicast_file(St, Path),
    St;

handle_file_changes(St, _Ref, {changed, Path, dir, _FileInfo, Files}) ->
    log("Changed (dir): ~s, ~w~n", [Path, Files]),
    lists:foldl(fun({Event, File}, StateIn) ->
			case Event of
			    added ->
 				add_file(StateIn, filename:join(Path, File), true);
 			    _ ->
 				StateIn
 			end
 		end, St, Files);

handle_file_changes(St, Ref, {error, Path, Type, enoent}) ->
    log("File has disappeared: ~s~n", [Path]),
    file_monitor:demonitor(Ref),
    multicast_delete(St, Path),
    St#state{files = lists:delete(Path, St#state.files)};

handle_file_changes(St, _Ref, X) ->
    log("Unhandled change event: ~w~n", [X]),
    St.

file_changed_message(File) ->
    {ok, Binary} = file:read_file(File),
    {ok, FileInfo} = file:read_file_info(File),
    {file_changed, File, FileInfo, Binary}.

%%% Send a file to a specific client
send_file(Client, File) ->
    log("Sending ~s to ~p~n", [File, Client]),
    Client ! file_changed_message(File).

%%% Send a file to all clients
multicast_file(St, File) ->
    case filelib:is_dir(File) of
	true ->
	    multicast_mkdir(St, File);
	false ->
	    multicast(St, file_changed_message(File))
    end.

multicast_delete(St, File) ->
    multicast(St, {delete, File}).
    

%%% Inform all clients that Dir should be created.
multicast_mkdir(St, Dir) ->
    multicast(St, {mkdir, Dir}).

multicast(St, Msg) ->
    lists:map(fun({Client, _}) -> Client ! Msg end, St#state.clients).

main(St) ->
    receive
	%% Register new client
	{register, Pid, SystemType} ->
	    main(add_client(Pid, SystemType, St));
	
	%% Client wants us to send them a file
	{send_file, Client, Path} ->
	    send_file(Client, Path),
	    main(St);

	{file_monitor, Ref, Event} ->
	    main(handle_file_changes(St, Ref, Event));

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

	{'EXIT', Pid, Reason} ->
	    log("EXIT(~p): ~w~n", [Pid, Reason]),
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
