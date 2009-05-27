package se.eskilson.buildengine.filemonitor.test;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Assert;
import org.junit.Test;

import se.eskilson.buildengine.filemonitor.FileEvent;
import se.eskilson.buildengine.filemonitor.IMonitoredFile;
import se.eskilson.buildengine.filemonitor.INotifyMonitoredFile;
import se.eskilson.buildengine.filemonitor.PollingMonitoredFile;
import se.eskilson.buildengine.filemonitor.FileEvent.Action;

public class FileMonitorTest {
	long pollTimeout = 10000;
	File tempdir = new File(this.getClass().getSimpleName() + ".tempdir");

	private void createTestDir(String... files) throws IOException {
		tempdir.mkdirs();
		for (String s : files) {
			File f = new File(tempdir, s);
			if (s.endsWith("/")) {
				f.mkdir();
			} else {
				f.createNewFile();
			}
		}
	}

	private void remove(File f) {
		if (f.isFile()) {
			f.delete();
		} else {
			for (File child : f.listFiles()) {
				remove(child);
			}
			f.delete();
		}

		Assert.assertFalse(f.exists());
	}

	@After
	public void removeTestDir() {
		remove(tempdir);
		Assert.assertFalse(tempdir.exists());
	}

	@Test
	public void testFileMonitor() throws IOException {
		createTestDir("a/", "a/foo.c", "a/bar.c");

		BlockingQueue<FileEvent> queue = new LinkedBlockingQueue<FileEvent>();
		IMonitoredFile mf = new PollingMonitoredFile(new File(tempdir, "a"),
				queue);
		mf.poll();

		List<FileEvent> list = new ArrayList<FileEvent>();
		queue.drainTo(list);
		assertEquals(3, list.size());

		list.clear();
		File foo = new File(tempdir, "a/foo.c");
		foo.setLastModified(foo.lastModified() + 5000);
		mf.poll();
		queue.drainTo(list);
		assertEquals(1, list.size());

		list.clear();
		remove(new File(tempdir, "a/foo.c"));
		mf.poll();
		queue.drainTo(list);
		assertEquals(1, list.size());
	}

	@Test
	public void testStartStopMonitoring() throws IOException,
			InterruptedException {
		createTestDir();

		BlockingQueue<FileEvent> queue = new LinkedBlockingQueue<FileEvent>();
		IMonitoredFile mf = new PollingMonitoredFile(tempdir, queue);

		mf.startMonitor();

		FileEvent event = queue.take();
		assertEquals(Action.Appeared, event.getAction());
		assertEquals(tempdir, event.getMonitoredFile().getFile());

		final File f = new File(tempdir, "dir_which_appears_after_a_while");

		Thread thread = new Thread() {
			@Override
			public void run() {
				try {
					Thread.sleep(3000);
					f.mkdir();
					Thread.sleep(3000);
					f.delete();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		};
		thread.start();

		event = queue.poll(pollTimeout, TimeUnit.MILLISECONDS);
		assertEquals(Action.Modified, event.getAction());
		assertEquals(tempdir, event.getMonitoredFile().getFile());

		event = queue.poll(pollTimeout, TimeUnit.MILLISECONDS);
		assertEquals(Action.Appeared, event.getAction());
		assertEquals(f, event.getMonitoredFile().getFile());

		event = queue.poll(pollTimeout, TimeUnit.MILLISECONDS);
		assertEquals(Action.Modified, event.getAction());
		assertEquals(tempdir, event.getMonitoredFile().getFile());

		event = queue.poll(pollTimeout, TimeUnit.MILLISECONDS);
		assertEquals(Action.Disappeared, event.getAction());
		assertEquals(f, event.getMonitoredFile().getFile());

		mf.stopMonitor();
		thread.join();
	}

	@Test
	public void testINotifyMonitoredFile() throws InterruptedException,
			IOException {
		createTestDir();

		BlockingQueue<FileEvent> queue = new LinkedBlockingQueue<FileEvent>();
		IMonitoredFile mf = new INotifyMonitoredFile(tempdir, queue);

		mf.startMonitor();
		final File f = new File(tempdir, "dir");
		Thread thread = new Thread() {
			@Override
			public void run() {
				f.mkdir();
				f.delete();
			}
		};
		thread.start();

		FileEvent event;

		event = queue.take();
		System.out.println("Event: " + event);

		event = queue.take();
		System.out.println("Event: " + event);

		mf.stopMonitor();
	}
}
