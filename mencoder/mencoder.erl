-module(mencoder).
-compile(export_all).

mplayer() ->
    "E:/MPlayer-1.0rc2/mplayer.exe".

mencoder() ->
    "E:/MPlayer-1.0rc2/mencoder.exe".

%% Opens a port, spawning Cmd. Sends port messages back to caller.
open_command_port(Cmd) ->
    From = self(),
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  io:format("Command: ~p~n", [Cmd]),
		  Port = open_port({spawn, string:join(Cmd, " ")},
				   [{line, 1024}]),
		  port_loop(Port, From)
	  end).

port_loop(Port, From) ->
    receive
	{'EXIT', _, Reason} ->
	    Port ! {self(), close},
	    From ! {command_terminated},
	    exit({port_terminated, Reason});
	{Port, {data, {eol, Data}}} ->
	    From ! {io, Data},
	    port_loop(Port, From)
    end.

receive_output(Fun, AccIn) ->
    receive
	{command_terminated} ->
	    AccIn;
	{io, Data} ->
	    AccOut = Fun(Data, AccIn),
	    receive_output(Fun, AccOut)
    end.

run_mplayer_command(Track, DvdDevice, Options, OutputReceiver, AccIn) ->
    open_command_port([mplayer(),
		       Track,
		       "-dvd-device",
		       DvdDevice] ++ Options),
    receive_output(OutputReceiver, AccIn).

crop_detect(Track, DvdDevice) ->
    F = fun(Data, AccIn) ->
		case regexp:first_match(Data, "crop=[0-9:]+") of
		    {match, Start, Length} ->
			CropStr = string:substr(Data, Start, Length),
			[CropStr|AccIn];
		    _ ->
			AccIn
		end
	end,
    CropStrs = run_mplayer_command(Track, DvdDevice,
				   ["-vo", "directx", "-ao", "null",
				    "-vf", "cropdetect,scale", 
				    "-sstep", "120", 
				    "-frames", "20"],
				   F, []),
    hd(CropStrs).

start() ->
    Crop = crop_detect("dvd://7", "F:"),
    erlang:display(Crop).


    
    

