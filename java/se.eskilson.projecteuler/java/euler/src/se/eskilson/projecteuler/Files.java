package se.eskilson.projecteuler;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class Files {
	public static String read(String name) throws IOException {
		return read(new File(name));
	}

	private static String read(File file) throws IOException {
		FileReader reader = new FileReader(file);
		int fileLen = (int) file.length();
		char buf[] = new char[fileLen];
		reader.read(buf);
		return new String(buf);
	}
}
