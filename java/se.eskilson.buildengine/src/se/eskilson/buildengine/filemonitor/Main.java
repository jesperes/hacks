package se.eskilson.buildengine.filemonitor;

import java.io.File;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class Main {
	static public void main(String[] argv) throws InterruptedException {
		File f = new File(argv[0]);

		BlockingQueue<FileEvent> queue = new LinkedBlockingQueue<FileEvent>();
		IMonitoredFile mf = new INotifyMonitoredFile(f, queue);
		mf.startMonitor();
		System.out.println("Monitoring " + f);
		while (true) {
			FileEvent event = queue.take();
			System.out.println(event);
		}
	}
}
