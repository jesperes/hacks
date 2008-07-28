%%%-------------------------------------------------------------------
%%% File    : inotify.erl
%%% Author  : Jesper Eskilson <jesperes@iar.se>
%%% Description : Wrapper around "inotifywait".
%%%
%%% Created : 22 Jul 2008 by Jesper Eskilson <jesperes@iar.se>
%%% -------------------------------------------------------------------
-module(inotify).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([monitor_dir/1]).
-export([test/0]).

-record(state, {}).

p(X) ->
    erlang:display(X).

%%====================================================================
%% API
%%====================================================================

monitor_dir(Dir) ->
    gen_server:call(?MODULE, {monitor_dir, Dir}).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, inotify}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

kill_external_process(Pid) ->
    io:format("Killing process: ~p~n", [Pid]),
    os:cmd("kill -9 " ++ Pid).

%% Spawns an inotifywait process, and pass Port and Pid (of the external
%% inotifywait process) to InotifyHandler.
spawn_inotify_process(Dir, InotifyHandler) ->
    Cmd = "/bin/sh -c 'echo $$ ; exec inotifywait -q -r -m " ++ Dir ++ "'",
    spawn_link(
      fun() ->
	      process_flag(trap_exit, true),
	      Port = open_port({spawn, Cmd},
			       [{line, 1024}]),
	      receive
		  {Port, {data, {eol, Pid}}} ->
		      InotifyHandler(Port, Pid)
	      end
      end).

%% Converts inotifywait output lines to messages to send back to client
convert_inotify_message(Data) ->
    case regexp:split(Data, " ") of
	{ok, [Dir, Events, File]} ->
	    {ok, EventList} = regexp:split(Events, ","),
	    AtomEventList = lists:map(fun list_to_atom/1,
				      lists:map(fun string:to_lower/1, 
						EventList)),
	    {Dir, File, AtomEventList};
	X ->
	    throw({inotify, unknown, X})
    end.

%% Receives events from the inotify port, and translates them into
%% messages to the process who sent the {monitor_dir, _} message.
inotify_loop(Port, Pid, From) ->
    %% p({inotify_loop, Port, Pid, From}),
    receive
	{_, {data, {eol, Data}}} ->
	    {Client, _} = From,
	    Client ! convert_inotify_message(Data),
	    inotify_loop(Port, Pid, From);
	{'EXIT', _, _} ->
	    io:format("Closing port: ~p~n", [Port]),
	    port_close(Port),
	    kill_external_process(Pid);
	X ->
	    throw({inotify, port, X})
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({monitor_dir, Dir}, From, State) ->
    Reply = ok,
    spawn_inotify_process(Dir,
      fun(Port, Pid) ->
	      inotify_loop(Port, Pid, From)
      end),
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

test() ->
    {ok, Pid} = start_link(),
    monitor_dir("/etc"),
    test_loop(10),
    exit(Pid, foo),
    init:stop().

test_loop(0) ->
    [];
test_loop(Msgs) ->
    receive
	X ->
	    p(X),
	    test_loop(Msgs - 1)
    end.
