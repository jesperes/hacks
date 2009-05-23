package se.eskilson.alfapet;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Set;
import java.util.TreeSet;

public class WordList {
	Set<String> words = new TreeSet<String>();

	public WordList(File wordfile) throws IOException {
		long start = System.nanoTime();

		BufferedReader reader = new BufferedReader(new InputStreamReader(
				new FileInputStream(wordfile)));
		String w;

		while ((w = reader.readLine()) != null) {
			words.add(w);
		}

		long elapsed = System.nanoTime() - start;
		System.out.format("Read %d words in %g seconds\n", words.size(),
				elapsed / 1000000000.0);
	}

	@Override
	public String toString() {
		return String.format("Word list (%d words)", words.size());
	}
}
