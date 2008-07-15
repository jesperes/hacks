-module(inotify).
-compile(export_all).

start(Dir) ->
    spawn(fun() ->
		  register(inotify, self()),
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "inotifywait -q -m -r " ++ Dir},
				   [{line, 1024}]),
		  loop(Port)
	  end).

stop() ->
    exit(whereis(inotify), stopping).

loop(Port) ->
    receive
	{'EXIT', _, Reason} ->
	    io:format("inotify port terminating: ~p~n", [Reason]),
	    Port ! {self(), close},
	    exit({port_terminated, Reason});
	{Port, {data, Data}} ->
	    io:format("~p~n", [Data]),
	    loop(Port)
    end.

   

    
				   
