%%%-------------------------------------------------------------------
%%% File    : ebt_server.erl
%%% Author  : Jesper Eskilson <jesperes@iar.se>
%%% Description : 
%%%
%%% Created : 20 Jul 2008 by Jesper Eskilson <jesperes@iar.se>
%%%-------------------------------------------------------------------
-module(ebt_server).
-compile(debug_info).
-compile(export_all).

-include("ebt_record.hrl").
-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0, set_srcdir/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {
	  srcdir = ""
	 }).


%% Interface functions
start() ->
    start_link().

stop() ->
    gen_server:call(?MODULE, stop).

set_srcdir(SrcDir) ->
    gen_server:call(?MODULE, {set_source, SrcDir}).
    

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ebt_server}, ?MODULE, [], []).

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
    %% start/setup database
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
handle_call({set_source, SrcDir}, _From, State) ->
    S0 = State#state{srcdir=SrcDir},
    %% 1. start an inotify process for SrcDir
    %% 2. scan SrcDir and update database
    %% NumberOfFiles is the number of files in the directory we're watching.
    NumberOfFiles = 0,
    Reply = {ok, SrcDir, NumberOfFiles},
    {reply, Reply, S0};

handle_call({update_file, File}, _From, State) ->
    %% update info for File
    %% no state change
    {reply, {updated, File}, State};

handle_call({delete_file, File}, _From, State) ->
    %% delete file from database
    %% no state change
    {reply, {deleted, File}, State}.

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

remove_filename_prefix(Prefix, Filename) ->
    string:substr(Filename, length(Prefix)+2).

default_table_defs() ->
    [{disc_only_copies, [node()]}].

tables() ->
    [file, updates].

table_def(file) ->
    [{attributes, record_info(fields, file)} | 
     default_table_defs()];
table_def(updates) ->
    [{attributes, record_info(fields, updates)} | 
     default_table_defs()].

create_table(TableName) ->
    Result = mnesia:create_table(TableName, table_def(TableName)),
    case Result of
	{atomic, ok} ->
	    TableName;
	Error ->
	    throw(Error)
    end.

exclude(F) ->
    case regexp:matches(F, "(\\.(git|svn|dll|pdb)|cmakebuild|Delivery|lib|Mnesia\\.)") of
	{match, []} ->
	    false;
	_ ->
	    true
    end.

update_file(Dir, F) ->
    Filename = remove_filename_prefix(Dir, F),
    Mtime = filelib:last_modified(F),
    case mnesia:read({file, Filename}) of
	[] ->
	    {ok, Binary} = file:read_file(F),
	    Record = #file{name = Filename, 
			   last_checked = now(),
			   mtime = Mtime,
			   contents = Binary},
	    mnesia:write(Record),
	    {added, Filename};
	[Record] ->
	    if Mtime > Record#file.mtime ->
		    %% File has been modified.
		    {ok, Binary} = file:read_file(F),
		    mnesia:delete({file, Filename}),
		    NewRecord = #file{name = Filename, 
				      last_checked = now(),
				      mtime = Mtime,
				      contents = Binary},
		    mnesia:write(NewRecord),
		    {updated, Filename, Mtime};
	       true ->
		    %% File has not been modified
		    {unchanged, Filename}
	    end
    end.

update_table_with_transaction(Dir, F) ->
    mnesia:transaction(fun() -> update_file(Dir, F) end).

update_table_visitor(Dir, F, AccIn) ->
    Excl = exclude(F),
    if Excl == false ->
	    case update_table_with_transaction(Dir, F) of
		{atomic, {added, _} = X0} ->
		    [X0|AccIn];
		{atomic, {unchanged, _}} ->
		    AccIn;
		{atomic, {updated, _File, _NewMtime} = X0} ->
		    [X0|AccIn];
		X ->
		    io:format("Weird: ~p~n", [X]),
		    AccIn
	    end;
       true ->
	    AccIn
    end.

%% Update the "file" tables, returns a list of all added files.
update_tables(Dir) ->
    filelib:fold_files(Dir, ".*", true,
		       fun(F, AccIn) -> 
			       update_table_visitor(Dir, F, AccIn)
		       end,
		       []).
    
%% Create the file table.
create_tables() ->
    lists:map(fun create_table/1, tables()).

%% start() ->
%%     {ok, Cwd} = file:get_cwd(),
%%     start(Cwd).

%% start(Dir) ->
%%     start_db(),
%%     Updates = update_tables(Dir),
%%     erlang:display(Updates),
%%     io:format("Updated: ~p~n", [length(Updates)]),
%%     init:stop().

start_db() ->
    mnesia:stop(),
    crypto:start(),
    case mnesia:create_schema([node()]) of
	ok ->
	    %% io:format("Created new Mnesia schema.~n"),
	    mnesia:start(),
	    create_tables();
	
	{error, {_Db, {already_exists, _}}} ->
	    %% io:format("Using Mnesia schema: ~p~n", [Db]),
	    mnesia:start();

	Error ->
	    throw(Error)
    end.
    
    
