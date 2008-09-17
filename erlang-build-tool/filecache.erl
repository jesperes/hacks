-module(filecache).
-export([start/2, stop/1]).
-include_lib("kernel/include/file.hrl").

-record(state, {srcdir, name}).

table_name(Name) ->
    list_to_atom(atom_to_list(Name) ++ ".filecache").

table_file(SrcDir, Name) ->
    filename:join(SrcDir, table_name(Name)).

no_compress(F) ->
    boolean_match([".*\.irp^",
		   ".*\.zip^"], F).

add_file(Name, F) ->
    case file:read_file(F) of
	{ok, Binary} ->
	    {ok, FileInfo} = file:read_file_info(F),
	    ShaSum = crypto:sha(Binary),
	    case no_compress(F) of
		true ->
		    dets:insert(Name, [{F, FileInfo, ShaSum, Binary}]);
		_ ->
		    ZBinary = zlib:gzip(Binary),
		    Size = size(Binary),
		    ZSize = size(ZBinary),
		    if ZSize >= Size ->
			    dets:insert(Name, [{F, FileInfo, ShaSum, Binary}]);
		       true ->
			    dets:insert(Name, [{F, FileInfo, ShaSum, ZBinary}])
		    end
	    end;
	{error, Reason} ->
	    io:format("Failed to add file ~s: ~p~n", [F, Reason])
    end.

file_exists(F) ->
    case file:read_file_info(F) of
	{ok, _} ->
	    true;
	_ ->
	    false
    end.

check_object({F, _FileInfo, _ShaSum, _Binary}) ->
    file_exists(F);
check_object(_) ->
    false.

read_file_shasum(F) ->
    case file:read_file(F) of
	{ok, Binary} ->
	    crypto:sha(Binary);
	_ ->
	    false
    end.

check_object_full({F, FileInfo, ShaSum, Binary} = Object) ->
    case check_object(Object) of
	true ->
	    ShaSum == read_file_shasum(F);
	_ ->
	    false
    end;
check_object_full(_) ->
    false.

%%% Removes files which do not exist.
purge_table(Name) ->
    Visitor =
	fun(Object) ->
		case check_object(Object) of
		    true ->
			true;
		    _ ->
			dets:delete(Name, Object)
		end,
		continue
	end,
    dets:traverse(Name, Visitor).

%% Matches if any one RE matches.
boolean_match([], _) ->
    false;
boolean_match([RE|Rest], String) ->
    Match = regexp:match(String, RE),
    case Match of
 	nomatch ->
	    boolean_match(Rest, String);
 	_ ->
 	    true
    end.

exclude(F) ->
    boolean_match([".*cmakebuild.*",
		   ".*test-artifacts.*",
		   ".*\.filecache.*",
		   "\.svn"], F).

start(SrcDir, Name) ->
    crypto:start(),
    dets:open_file(table_name(Name), 
		   [{auto_save, 5000},
		    {file, table_file(SrcDir, Name)}]),
    purge_table(table_name(Name)),
    Visitor =
	fun(F, AccIn) ->
		case exclude(F) of
		    true ->
			AccIn;
		    _ ->
			case dets:lookup(table_name(Name), F) of
			    {error, Reason} ->
				erlang:display({dets_error, Reason});
			    [] ->
				add_file(table_name(Name), F);
			    Objects ->
				lists:map(
				  fun(O) ->
					  case check_object_full(O) of
					      false ->
						  {F, _, _, _} = O,
						  io:format("Refreshing table: ~s~n", [F]);
					      true ->
						  true
					  end
				  end, Objects)
			end,
			AccIn + 1
		end
	end,
    NumFiles = filelib:fold_files(SrcDir, ".*", true, Visitor, 0),
    io:format("Files in table: ~w~n", [NumFiles]).

stop(Name) ->    
    dets:close(table_name(Name)).
    
%%%     Pid = 
%%% 	spawn(fun() ->
%%% 		      loop(#state{srcdir = SrcDir, name = Name})
%%% 	      end),
%%%     Pid.

loop(St) ->
    receive
	X ->
	    erlang:display(X),
	    loop(St)
    end.
