-module(mencoder).
-compile(export_all).
-include_lib("kernel/include/file.hrl").

-record(profile, 
	{vbitrate = 1200, 
	 abitrate = copy, 
	 crop = auto,
	 scale = false,
	 pp = default }).

mplayer() ->
    os:find_executable("mplayer").

mencoder() ->
    os:find_executable("mencoder").

default_mencoder_options() ->
    ["-priority", "idle",
     "-ffourcc", "XVID",
     "-idx",
     "-frames", "500"].

dvd_device_option(DvdDevice) when is_list(DvdDevice) ->
    ["-dvd-device", DvdDevice];
dvd_device_option(_) ->
    [].

profile(dvd) ->
    #profile{}.

%% Opens a port, spawning Cmd. Sends port messages back to caller.
open_command_port(Cmd) ->
    From = self(),
    spawn(fun() ->
		  process_flag(trap_exit, true),
		  %% io:format("Command: ~p~n", [Cmd]),
		  Port = open_port({spawn, string:join(Cmd, " ")},
				   [{line, 1024}, exit_status, stderr_to_stdout]),
		  port_loop(Port, From)
	  end).

port_loop(Port, From) ->
    receive
	{Port, {exit_status, Status}} ->
	    %% io:format("Exit status: ~p~n", [Status]),
	    Port ! {self(), close},
	    From ! {command_terminated},
	    exit({port_terminated, Status});
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
    after
	250 ->
	    AccOut = Fun(poll, AccIn),
	    receive_output(Fun, AccOut)
    end.

run_command(Command, Track, DvdDevice, Options, OutputReceiver, AccIn) ->
    open_command_port([Command, Track] ++ 
		      dvd_device_option(DvdDevice) ++ 
		      Options),
    receive_output(OutputReceiver, AccIn).

%% Autodetect crop-area for a given track/device.
%% Track can be any input URI accepted by mplayer.
crop_detect(Track, DvdDevice) ->
    io:format("Crop detecting...~n"),
    F = fun(Data, AccIn) ->
		case Data of
		    poll -> AccIn;
		    _ ->
			case regexp:first_match(Data, "crop=[0-9:]+") of
			    {match, Start, Length} ->
				CropStr = string:substr(Data, Start, Length),
				[CropStr|AccIn];
			    _ ->
				AccIn
			end
		end
	end,
    CropStrs = run_command(mplayer(), Track, DvdDevice,
			   ["-vo", "directx", "-ao", "null",
			    "-vf", "cropdetect,scale", 
			    "-sstep", "120", 
			    "-frames", "10"],
			   F, []),
    hd(CropStrs).


video_codec_pass_options(pass1, _Profile) ->
    ":turbo:vpass=1";
video_codec_pass_options(pass2, _Profile) ->
    ":mbd=2:trell:v4mv:vpass=2".

video_codec_options(Pass, Profile) ->
    ["-ovc", "lavc", "-lavcopts",
     "vcodec=mpeg4:autoaspect:" ++
     "vbitrate=" ++ integer_to_list(Profile#profile.vbitrate) ++
     case Profile#profile.abitrate of
	 copy -> 
	     "";
	 Abitrate ->
	     ":acodec=mp3:abitrate=" ++ integer_to_list(Abitrate)
     end ++
     video_codec_pass_options(Pass, Profile)
    ].

video_filters(_Pass, Profile, Track, DvdDevice) ->
    ["-vf-pre",
     string:join(
       [
	case Profile#profile.crop of
	    auto ->
		crop_detect(Track, DvdDevice);
	    CropFilter ->
		CropFilter
	end,
	case Profile#profile.pp of
	    default ->
		"pp=default";
	    Filter -> Filter
	end,
	case Profile#profile.scale of
	    false ->
		"scale";
	    Scale -> Scale
	end,
	"harddup"
       ], ",")].

audio_codec_options(_Pass, Profile) ->
    case Profile#profile.abitrate of
	copy ->
	    ["-oac", "copy"];
	_ ->
	    ["-oac", "lavc"]
    end.

encode(Pass, Profile, Track, DvdDevice, OutputFile) ->
    io:format("Encoding: ~p~n", [{Pass, Track, DvdDevice, Profile}]),
    F = fun(Data, AccIn) ->
		FileSize = AccIn,
		case Data of
		    poll -> 
			{ok, FileInfo} = file:read_file_info(OutputFile),
			NewFileSize = FileInfo#file_info.size,
			if NewFileSize /= FileSize ->
				FileSizeMb = NewFileSize / (1024 * 1024),
				io:format("~s: ~.2f Mb~n", [OutputFile, FileSizeMb]),
				NewFileSize;
			   true ->
				FileSize
			end;
		    _ ->
			case regexp:first_match(Data, "(videocodec:|audiocodec:)") of
			    {match, _, _} ->
				io:format("~s~n", [Data]) ;
			    _ ->
				nil
			end,
			FileSize
		end
	end,
    run_command(mencoder(), Track, DvdDevice,
		["-o", OutputFile] ++
		default_mencoder_options() ++
		video_codec_options(Pass, Profile) ++
		audio_codec_options(Pass, Profile) ++
		video_filters(Pass, Profile, Track, DvdDevice),
		F, 0).

rip(ProfileName, Track, DvdDevice, OutputFile) ->
    Profile = profile(ProfileName),
    encode(pass1, Profile, Track, DvdDevice, OutputFile).

rip() ->
    rip(dvd, "dvd://7", "F:", "cg.avi").

