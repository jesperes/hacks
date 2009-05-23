package se.eskilson.buildengine.filemonitor;

import java.util.Date;

public class FileEvent {

	static public enum Action {
		Appeared, // A file/directory is discovered
		Modified,
		Disappeared;
	};

	private final Action action;
	private final MonitoredFile file;
	private final Date timeStamp;

	public FileEvent(MonitoredFile file, Action action) {
		this.action = action;
		this.file = file;
		this.timeStamp = new Date();
	}

	@Override
	public String toString() {
		return String.format("FileEvent<%s, %s, %s>", action, file, timeStamp);
	}
}
