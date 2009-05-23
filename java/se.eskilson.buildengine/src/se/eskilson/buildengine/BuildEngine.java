package se.eskilson.buildengine;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BuildEngine {

	static Set<String> excludeList = new TreeSet<String>();

	static {
		excludeList.add("/usr/bin");
		excludeList.add("/usr/lib");
		excludeList.add("/usr/share");
		excludeList.add("/lib");
		excludeList.add("/var");
		excludeList.add("/etc");
		excludeList.add("/proc");
		excludeList.add("/tmp");
		excludeList.add("/dev");
	}

	public static class StdoutReader extends Thread {
		private final InputStream stream;

		private final Set<File> readFiles = new TreeSet<File>();
		private final Set<File> writtenFiles = new TreeSet<File>();

		public StdoutReader(Process p) {
			stream = p.getInputStream();
		}

		public Set<File> getReadFiles() {
			return readFiles;
		}

		public Set<File> getWrittenFiles() {
			return writtenFiles;
		}

		private boolean exclude(String s) {
			for (String exclude : excludeList) {
				if (s.startsWith(exclude)) {
					return true;
				}
			}
			return false;
		}

		@Override
		public void run() {
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					stream));
			String s;
			Pattern p = Pattern.compile("^.*\"([^\"]+)\".*$");
			Set<String> rfiles = new TreeSet<String>();
			Set<String> wfiles = new TreeSet<String>();
			try {
				while ((s = reader.readLine()) != null) {
					Matcher m = p.matcher(s);
					if (m.matches()) {
						if (s.contains("O_RDWR")) {
							wfiles.add(m.group(1));
						} else {
							rfiles.add(m.group(1));
						}
					} else {
						// System.out.println("Non-matched output: " + s);
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
			}

			for (String filename : rfiles) {
				if (exclude(filename))
					continue;

				File f = new File(filename);
				if (f.exists()) {
					readFiles.add(f);
				}
			}

			for (String filename : wfiles) {
				File f = new File(filename);
				if (f.exists()) {
					writtenFiles.add(f);
				}
			}
		}
	}

	public static void main(String[] args) throws IOException,
			InterruptedException {
		BuildEngine engine = new BuildEngine();
		// engine.runCommand("strace", "-f", "-e", "trace=open", "gcc", "foo.c",
		// "-o", "foo.out");
		// engine.runCommand("strace", "-f", "-e", "trace=open", "apt-cache",
		// "search", "git");
		engine.runCommand("strace", "-f", "-e", "trace=open", "synaptic");
	}

	private void runCommand(String... args) throws IOException,
			InterruptedException {
		ProcessBuilder pb = new ProcessBuilder();
		pb.command(args);
		pb.redirectErrorStream(true);

		System.out.println("Command: " + Arrays.asList(args));
		Process p;
		long start = System.nanoTime();
		StdoutReader reader = new StdoutReader(p = pb.start());
		reader.start();
		reader.join();
		System.out.println("Exit code: " + p.waitFor());
		long stop = System.nanoTime();

		System.out.format("Time taken %g seconds.\n",
				(stop - start) / 1000000000.0);
		for (File f : reader.getWrittenFiles()) {
			System.out.println("Written file: " + f);
		}
		for (File f : reader.getReadFiles()) {
			System.out.println("Read file: " + f);
		}
	}
}
