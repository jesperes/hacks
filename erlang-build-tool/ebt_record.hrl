-record(file, 
	{name, 				     %% relative name within tree
	 sha1sum,			     %% file checksum
	 treename,			     %% logical name of tree
	 last_checked,			     %% time of last check
	 contents			     %% binary blob with file contents
	 }).

-record(updates,
	{time,
	 what,
	 files_changed}).
