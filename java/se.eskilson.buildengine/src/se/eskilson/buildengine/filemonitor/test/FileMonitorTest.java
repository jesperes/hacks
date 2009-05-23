package se.eskilson.buildengine.filemonitor.test;

import java.io.File;

import org.junit.Assert;
import org.junit.Test;

import se.eskilson.buildengine.filemonitor.MonitoredFile;

public class FileMonitorTest {
	@Test
	public void testFileMonitor() {
		MonitoredFile mf = new MonitoredFile(new File("filemonitor_tests"),
				null);
		mf.rescan(null);
		mf.rescan(null);
		Assert.assertNotNull(mf);
	}
}
