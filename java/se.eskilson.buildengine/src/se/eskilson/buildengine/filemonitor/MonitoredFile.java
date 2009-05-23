package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class MonitoredFile implements IMonitoredFile {
	File file;
	Date lastModified;
	Date lastCheck;

	Map<String, MonitoredFile> children = new HashMap<String, MonitoredFile>();

	public MonitoredFile(File file, Collection<IFileListener> listeners) {
		this.file = file;
		if (!file.exists()) {
			throw new IllegalArgumentException("file does not exist: " + file);
		}
	}

	@Override
	public void rescan(Collection<IFileListener> listeners) {
		lastModified = new Date(file.lastModified());
		lastCheck = new Date();

		System.out.println("Scanning: " + file);
		if (file.isDirectory()) {
			for (String s : file.list()) {
				if (children.containsKey(s)) {
					System.out.println(this + ": already contains " + s);
					children.get(s).rescan(listeners);
				} else {
					System.out.println(this + ": new file " + s);
					notifyListeners(listeners, new File(s), FileEvent.Appeared);
					MonitoredFile mf = new MonitoredFile(new File(file, s),
							listeners);
					children.put(s, mf);
					mf.rescan(listeners);
				}
			}
		}
	}

	@Override
	public void notifyListeners(Collection<IFileListener> listeners, File f,
			FileEvent event) {
		if (listeners == null)
			return;

		for (IFileListener l : listeners) {
			l.fileChanged(f, event);
		}
	}

	@Override
	public String toString() {
		return String.format("MonitoredFile<%s %s>", file, this.lastModified);
	}
}
