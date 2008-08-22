-module(ebt_server).
-compile(debug_info).
-compile(export_all).

-record(state, {db}).

start() ->
    start(".").

start([SrcDir|_]) ->
    code:add_patha("/home/jesperes/eunit/ebin"),
    file:set_cwd(SrcDir),
    Db = ebt_db:start(),
    file_monitor:start(),
    St = #state{db = Db},
    spawn(fun() ->
		  register(ebt_server, self()),
		  monitor_tree(self()),
		  main(St) 
	  end).

get_excludes() ->
    "(\\.(git|svn|dll|pdb)|cmakebuild|Delivery|lib|Mnesia\\.)".

get_monitored_files(Dir) ->
    {ok, ParsedRE} = regexp:parse(get_excludes()),
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
    filelib:fold_files(Dir, ".*", true, F, {[], 0, 0}).

monitor_tree(Receiver) ->
    statistics(wall_clock),
    
    {ok, Cwd} = file:get_cwd(),
    io:format("Scanning directory for files to monitor: ~p~n", [Cwd]),

    {Files, NumFiles, NumExcl} = get_monitored_files("."),
    io:format("Files to monitor: ~p (~p files excluded)~n", [NumFiles, NumExcl]),
    
    lists:map(fun(F) -> file_monitor:monitor_file(F, Receiver) end, Files),
    {_, Time} = statistics(wall_clock),
    
    io:format("Setup phase took ~g seconds.~n", [Time/1000]).


main(St) ->
    receive
	{getpid, From} ->
	    From ! {pid, self()},
	    main(St);
	{file_monitor, _Ref, {exists, Path, _Type, _FileInfo, _}} ->
	    St#state.db ! {exists, Path},
	    main(St);
	{file_monitor, _Ref, {changed, Path, _Type, _FileInfo, _}} ->
	    St#state.db ! {changed, Path},
	    main(St);
	{file_monitor, _Ref, {error, Path, _Type, _Info}} ->
	    erlang:display({error, Path}),
	    main(St);
	{add_table_copy, Node, From} ->
	    erlang:display({add_table_copy, Node, From}),
	    St#state.db ! {add_table_copy, Node, From},
	    main(St);
	X ->
	    erlang:display({unknown, X}),
	    main(St)
    end.
