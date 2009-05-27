package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.Date;
import java.util.concurrent.BlockingQueue;

public class AbstractMonitoredFile implements IMonitoredFile {

	private final BlockingQueue<FileEvent> queue;
	private final File file;

	public AbstractMonitoredFile(File file, BlockingQueue<FileEvent> queue) {
		this.file = file;
		this.queue = queue;
	}

	@Override
	public File getFile() {
		return file;
	}

	@Override
	public BlockingQueue<FileEvent> getQueue() {
		return queue;
	}

	@Override
	public String toString() {
		return String.format("MonitoredFile<%s, mtime=%s>", file, new Date(file
				.lastModified()));
	}

	@Override
	public void poll() {

	}

	@Override
	public void startMonitor() {

	}

	@Override
	public void stopMonitor() throws InterruptedException {

	}

}
