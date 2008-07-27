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
    Parent = self(),
    Cmd = "inotifywait -q -m -r ",
    spawn_link(fun() ->
		       process_flag(trap_exit, true),
		       Port = open_port({spawn, Cmd ++ Dir},
					[{line, 1024}]),
		       loop_porthandler(Port, Parent, From)
	       end),
    {reply, Reply, State}.

%% Receives events from the inotify port, and translates them into
%% messages to the process who sent the {monitor_dir, _} message.
loop_porthandler(Port, Parent, From) ->
    receive
	{Port, {data, {eol, Data}}} ->
	    case regexp:split(Data, " ") of
		{ok, [Dir, Events, File]} ->
		    {ok, EventList} = regexp:split(Events, ","),
		    AtomEventList = lists:map(fun list_to_atom/1,
					      lists:map(fun string:to_lower/1, 
							EventList)),
		    Reply = {Dir, File, AtomEventList},
		    {Origin, _} = From,
		    Origin ! Reply,
		    loop_porthandler(Port, Parent, From);
		X ->
		    io:format("Unexpected match result from inotify process: ~p~n", [X])
	    end;
	{'EXIT', _, _} ->
	    io:format("Closing port: ~p~n", [Port]),
	    port_close(Port);
	X ->
	    io:format("Unexpected from inotify process: ~p~n", [X]),
	    loop_porthandler(Port, Parent, From)
    end.

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
terminate(Reason, _State) ->
    erlang:display(Reason),
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
    exit(Pid, stop),
    init:stop().

test_loop(Msgs) ->
    if Msgs > 0 ->
	    receive
		X ->
		    erlang:display(X),
		    test_loop(Msgs - 1)
	    end;
       true ->
	    []
    end.

	   
    

	  
