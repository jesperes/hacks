package se.eskilson.buildengine.filemonitor.test;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.junit.After;
import org.junit.Assert;
import org.junit.Test;

import se.eskilson.buildengine.filemonitor.FileEvent;
import se.eskilson.buildengine.filemonitor.MonitoredFile;

public class FileMonitorTest {
	File tempdir = new File(this.getClass().getSimpleName() + ".tempdir");

	private void createTestDir(String... files) throws IOException {
		tempdir.mkdirs();
		for (String s : files) {
			File f = new File(tempdir, s);
			if (s.endsWith("/")) {
				System.out.println("Created " + f);
				f.mkdir();
			} else {
				System.out.println("Created " + f);
				f.createNewFile();
			}
		}
	}

	private void remove(File f) {
		System.out.println("Deleting " + f);
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

	private void showList(String msg, Collection<FileEvent> list) {
		System.out.println("<<<");
		System.out.println(msg);
		for (FileEvent e : list) {
			System.out.println("Event: " + e);
		}
		System.out.println(">>>");
	}

	@Test
	public void testFileMonitor() throws IOException {
		createTestDir("a/", "a/foo.c", "a/bar.c");

		BlockingQueue<FileEvent> queue = new LinkedBlockingQueue<FileEvent>();
		MonitoredFile mf = new MonitoredFile(new File(tempdir, "a"), queue);
		mf.poll();

		List<FileEvent> list = new ArrayList<FileEvent>();
		queue.drainTo(list);
		showList("initial poll", list);
		assertEquals(3, list.size());

		list.clear();
		File foo = new File(tempdir, "a/foo.c");
		foo.setLastModified(foo.lastModified() + 5000);
		mf.poll();
		queue.drainTo(list);
		showList("after timestamping " + foo, list);
		assertEquals(1, list.size());

		list.clear();
		remove(new File(tempdir, "a/foo.c"));
		mf.poll();
		queue.drainTo(list);
		showList("after removing file", list);
		assertEquals(1, list.size());
	}
}
