-module(node).
-export([ add_dependency/2,
	  add_dependencies/2,
	  status/1,
	  main_loop/1,
	  create_new/4 ]).

%% type: one of {file, ...}
%% status: one of {up_to_date, out_of_date, unknown}
%% timestamp: timestamp of last target check
%% target: if type == file, then target is the filename.
-record(node, {type, 
	       status,
	       timestamp,
	       target, 
	       deps = [], 
	       num_children = 0,
	       build,
	       builder}).

status(NodePid) ->
    NodePid ! { status, self() },
    receive
	{status, Status} ->
	    Status
    end.

add_dependency(NodePid, Dep) ->
    NodePid ! { add_dep, Dep }.

add_dependencies(NodePid, Deps) ->
    NodePid ! { add_deps, Deps }.

main_loop(Node) ->
    receive
	finished ->
	    io:format("~p exiting.~n", [self()]),
	    true;

	build ->
	    BuiltNode = build(Node),
	    main_loop(BuiltNode);

	{status, From} ->
	    From ! {status, Node},
	    main_loop(Node);

	{add_dep, Dep} ->
	    io:format("~p~n", [Dep]),
	    Dep ! {added_child, self()},
	    Node0 = Node#node{deps = [Dep | Node#node.deps]},
	    main_loop(Node0);
	
	{add_deps, Deps} ->
	    [Dep ! {added_child, self()} || Dep <- Deps],
	    Node0 = Node#node{deps = Node#node.deps ++ Deps},
	    main_loop(Node0);
	
	{added_child, _Child} ->
	    Node0 = Node#node{num_children = Node#node.num_children + 1},
	    main_loop(Node0);

	Msg ->
	    io:format("~p got unknown message: ~p~n", [self(), Msg])
    end.
	    
build(Node) ->
    io:format("Building node: ~p~n", [Node]).

create_new(Env, Type, Target, Builder) ->
    NodeData = #node{type = Type,
		     status = unknown,
		     target = Target,
		     deps = [],
		     build = Env,
		     builder = Builder},
    spawn(node, main_loop, [NodeData]).



