-module(build).
-export([add_toplevel/1,
	 add_toplevel/2,
	 add_subdirectory/2]).

add_toplevel(BuildScript) ->
    add_toplevel(BuildScript, filename:dirname(BuildScript)).

add_toplevel(BuildScript, BuildDir) ->
    SourceDir = filename:dirname(BuildScript),
    case c:c(BuildScript) of
	{ok, Module} ->
	    build_toplevel(Module, SourceDir, BuildDir);
	Result ->
	    throw(Result)
    end.

%% BuildScript must be relative to the parents source directory.
add_subdirectory(ParentEnv, BuildScript) ->
    SourceDir = filename:dirname(BuildScript),
    File = filename:join(env:source_dir(ParentEnv), BuildScript),
    case c:c(File) of
	{ok, Module} ->
	    build_subdir(ParentEnv, Module,
			 filename:join(env:source_dir(ParentEnv),
				       SourceDir),
			 filename:join(env:build_dir(ParentEnv),
				       SourceDir));
	Result ->
	    throw(Result)
    end.

build_subdir(ParentEnv, Module, SourceDir, BuildDir) ->
    Env = env:make_env(ParentEnv, SourceDir, BuildDir),
    Module:build(Env).

build_toplevel(Module, SourceDir, BuildDir) ->
    Env = env:make_env(SourceDir, BuildDir),
    Module:build(Env).
