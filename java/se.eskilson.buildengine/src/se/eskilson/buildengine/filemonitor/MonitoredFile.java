package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;

import se.eskilson.buildengine.filemonitor.FileEvent.Action;

/**
 * Monitors a file or directory (including sub-directories), and adds
 * file-change events to a queue.
 * 
 * @author jesperes
 */
public class MonitoredFile {
	File file;
	Date lastModified;
	Date lastCheck;

	/*
	 * A map containing the children of this directory. The File objects used as
	 * keys are relative to this file.
	 */
	Map<File, MonitoredFile> children = new HashMap<File, MonitoredFile>();
	private final BlockingQueue<FileEvent> queue;

	public MonitoredFile(File file, BlockingQueue<FileEvent> queue) {
		this.file = file;
		this.queue = queue;
		this.lastModified = new Date(file.lastModified());

		if (!file.exists()) {
			throw new IllegalArgumentException("file does not exist: " + file);
		}

		queue.add(new FileEvent(this, Action.Appeared));
		poll();
	}

	public final void poll() {
		lastCheck = new Date();
		long currentLastModified = file.lastModified();

		// Has the file itself been modified. If so, trigger "modified" event.
		if (currentLastModified > lastModified.getTime()) {
			queue.add(new FileEvent(this, Action.Modified));
		}

		lastModified = new Date(currentLastModified);

		// If file is a directory, poll its children.
		if (file.isDirectory()) {
			for (String s : file.list()) {
				File child = new File(s);
				if (children.containsKey(child)) {
					children.get(child).poll();
				} else {
					// A new file has appeared.
					children.put(child, new MonitoredFile(new File(file, s),
							queue));
				}
			}

			Collection<File> deletedChildren = new ArrayList<File>();

			for (File file : children.keySet()) {
				MonitoredFile mf = children.get(file);
				if (!mf.file.exists()) {
					queue.add(new FileEvent(mf, Action.Disappeared));
					deletedChildren.add(file);
				}
			}

			for (File file : deletedChildren) {
				children.remove(file);
			}
		}
	}

	@Override
	public String toString() {
		return String.format("MonitoredFile<%s, %s>", file, this.lastModified);
	}
}
