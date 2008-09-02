-module(source_config).
-compile(export_all).

get_excludes() ->
    "(\\.(git|svn))".

automatic_build() ->
    true.

build_command(ModifiedFiles) ->    
    "make".

build_dir() ->
    ".".
