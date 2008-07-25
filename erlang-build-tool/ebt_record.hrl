-record(file, 
	{name, 				     %% relative name within tree
	 sha1sum,			     %% file checksum
	 last_checked,			     %% time of last check
	 mtime,				     %% last modification time of file
	 contents			     %% binary blob with file contents
	 }).

-record(updates,
	{time,
	 what,
	 files_changed}).
