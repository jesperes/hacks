-module(porttest).
-compile(export_all).

start() ->
    Pid = 
	spawn_link(
	  fun() ->
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "/bin/sh -c 'echo $$ ; exec inotifywait -q -r -m /etc'"}, []),
		  receive
		      {_, {data, PidStr}} ->
			  Pid = string:substr(PidStr, 1, length(PidStr)-1),
			  loop(Port, Pid)
		  end
	  end),
    receive
	after 5000 ->
		exit(Pid, timeout)
	end.

loop(Port, Pid) ->
    receive 
	{'EXIT', _, Reason} ->
	    erlang:display({killing, Reason, Pid}),
	    os:cmd("kill -9 " ++ Pid),
	    init:stop();
	X ->
	    erlang:display({Pid, X}),
	    loop(Port, Pid)
    end.

