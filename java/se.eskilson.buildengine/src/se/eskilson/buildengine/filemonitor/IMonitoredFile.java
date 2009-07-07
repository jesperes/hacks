package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.concurrent.BlockingQueue;

public interface IMonitoredFile {

	public File getFile();

	public BlockingQueue<FileEvent> getQueue();

	public void poll();

	/**
	 * Monitor the file for changes,
	 */
	public void startMonitor();

	public void stopMonitor() throws InterruptedException;

}
