package se.eskilson.buildengine.filemonitor;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import se.eskilson.buildengine.filemonitor.FileEvent.Action;

/*
 * Implements file monitoring using the "inotifywait" tool.
 */
public class INotifyMonitoredFile extends AbstractMonitoredFile {

	private Process inotifywait;
	private StdHandler stdout;

	private final Map<File, IMonitoredFile> fileMap = new HashMap<File, IMonitoredFile>();

	private static class StdHandler extends Thread {
		private final BufferedReader reader;
		private final BlockingQueue<FileEvent> queue;
		private final Map<File, IMonitoredFile> fileMap;

		public StdHandler(Map<File, IMonitoredFile> fileMap,
				BlockingQueue<FileEvent> queue, InputStream str) {
			this.reader = new BufferedReader(new InputStreamReader(str));
			this.queue = queue;
			this.fileMap = fileMap;
		}

		@Override
		public void run() {
			String s;

			Pattern p = Pattern.compile("^(.*);(.*);(.*)$");

			try {
				while ((s = reader.readLine()) != null) {
					Matcher m = p.matcher(s);
					if (m.matches()) {
						String dir = m.group(1);
						String file = m.group(2);
						String events = m.group(3);

						File f;
						if (file == null || file.length() == 0) {
							f = new File(dir);
						} else {
							f = new File(dir, file);
						}

						IMonitoredFile mf = fileMap.get(f);
						if (mf == null) {
							mf = new AbstractMonitoredFile(f, queue);
							fileMap.put(f, mf);
						}

						if (events.contains("CREATE")) {
							queue.add(new FileEvent(mf, Action.Appeared));
						} else if (events.contains("DELETE")) {
							queue.add(new FileEvent(mf, Action.Disappeared));
							fileMap.remove(f);
						} else if (events.contains("MODIFY")) {
							queue.add(new FileEvent(mf, Action.Modified));
						}
					} else {
						System.out.println("Unmatched output: " + s);
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public INotifyMonitoredFile(File file, BlockingQueue<FileEvent> queue) {
		super(file, queue);

		if (!System.getProperty("os.name").contains("Linux"))
			throw new UnsupportedOperationException(
					"This class is only supported on Linux.");
	}

	@Override
	public void poll() {
		// nop
	}

	@Override
	public void startMonitor() {
		ProcessBuilder pb = new ProcessBuilder();

		String format = "%w;%f;%,e";
		pb.command("inotifywait", "-e", "modify", "-e", "create", "-e",
				"delete", "-r", "-m", "--format", format, getFile().toString());
		pb.redirectErrorStream(true);
		try {
			inotifywait = pb.start();
		} catch (IOException e) {
			e.printStackTrace();
		}

		stdout = new StdHandler(fileMap, getQueue(), inotifywait
				.getInputStream());
		stdout.start();
	}

	@Override
	public void stopMonitor() throws InterruptedException {
		inotifywait.destroy();
		int code = inotifywait.waitFor();
		stdout.join();
		System.out.println("exit code: " + code);
	}
}
