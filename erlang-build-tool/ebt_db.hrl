-include_lib("kernel/include/file.hrl").

-record(file, 
	{name, 				     %% filename
	 file_info,			     %% file_info as returned by file:read_file_info()
	 contents			     %% binary blob with file contents
	}).
