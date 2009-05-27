package se.eskilson.buildengine.filemonitor;

import java.util.Date;

public class FileEvent {

	static public enum Action {
		Appeared, // A file/directory is discovered
		Modified,
		Disappeared;
	};

	private final Action action;
	private final IMonitoredFile file;
	private final Date timeStamp;

	public FileEvent(IMonitoredFile file, Action action) {
		this.action = action;
		this.file = file;
		this.timeStamp = new Date();
	}

	public Action getAction() {
		return action;
	}

	public IMonitoredFile getMonitoredFile() {
		return file;
	}

	public Date getTimeStamp() {
		return timeStamp;
	}

	@Override
	public String toString() {
		return String.format("FileEvent<%s, %s, event-timestamp=%s>", action,
				file, timeStamp);
	}
}
