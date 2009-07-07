package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import se.eskilson.buildengine.filemonitor.FileEvent.Action;

/**
 * Monitors a file by polling it at a given interval.
 */
public class PollingMonitoredFile extends AbstractMonitoredFile {
	Date lastModified;
	Date lastCheck;
	CountDownLatch stopSignal = null;
	long pollInterval = 2000;
	TimeUnit pollTimeUnit = TimeUnit.MILLISECONDS;

	/*
	 * A map containing the children of this directory. The File objects used as
	 * keys are relative to this file.
	 */
	Map<File, PollingMonitoredFile> children = new HashMap<File, PollingMonitoredFile>();
	Thread thread;

	public void setPollingInterval(long msecs) {
		pollInterval = msecs;
	}

	public PollingMonitoredFile(File file, BlockingQueue<FileEvent> queue) {
		super(file, queue);
		this.lastModified = new Date(file.lastModified());

		if (!file.exists()) {
			throw new IllegalArgumentException("file does not exist: " + file);
		}

		queue.add(new FileEvent(this, Action.Appeared));
		poll();
	}

	@Override
	public final void poll() {
		lastCheck = new Date();
		long currentLastModified = getFile().lastModified();

		// Has the file itself been modified. If so, trigger "modified" event.
		if (currentLastModified > lastModified.getTime()) {
			getQueue().add(new FileEvent(this, Action.Modified));
		}

		lastModified = new Date(currentLastModified);

		// If file is a directory, poll its children.
		if (getFile().isDirectory()) {

			for (String s : getFile().list()) {
				File child = new File(s);
				if (children.containsKey(child)) {
					children.get(child).poll();
				} else {
					// A new file has appeared.
					children.put(child, new PollingMonitoredFile(new File(
							getFile(), s), getQueue()));
				}
			}

			Collection<File> deletedChildren = new ArrayList<File>();

			for (File file : children.keySet()) {
				PollingMonitoredFile mf = children.get(file);
				if (!mf.getFile().exists()) {
					getQueue().add(new FileEvent(mf, Action.Disappeared));
					deletedChildren.add(file);
				}
			}

			for (File file : deletedChildren) {
				children.remove(file);
			}
		}
	}

	/**
	 * Monitor the file for changes,
	 */
	@Override
	public void startMonitor() {
		stopSignal = new CountDownLatch(1);
		thread = new Thread() {
			@Override
			public void run() {
				try {
					while (stopSignal.await(pollInterval, pollTimeUnit) == false) {
						poll();
					}
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		};

		thread.start();
	}

	@Override
	public void stopMonitor() throws InterruptedException {
		if (stopSignal == null) {
			throw new IllegalStateException(
					"stopMonitor called without having called startMonitor first");
		}

		stopSignal.countDown();
		thread.join();
	}

	@Override
	public String toString() {
		return String.format("PollingMonitoredFile<%s, %s>", getFile(),
				this.lastModified);
	}
}
