-module(env).
-export([make_env/2,
	 make_env/3,
	 source_dir/1,
	 build_dir/1]).

-record(env,
	{name = "Default",
	 source_dir,
	 build_dir,
	 parent,
	 builders = []}).

make_env(SourceDir, BuildDir) ->
    #env{source_dir = SourceDir, build_dir = BuildDir, parent = root}.

make_env(ParentBuild, SourceDir, BuildDir) ->
    #env{source_dir = SourceDir, build_dir = BuildDir, parent = ParentBuild}.

source_dir(Env) ->
    Env#env.source_dir.

build_dir(Env) ->
    Env#env.build_dir.



    
