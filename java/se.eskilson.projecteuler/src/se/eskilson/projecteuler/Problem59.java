package se.eskilson.projecteuler;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Problem59 {

	private static List<Integer> readCipherFile(File file) throws IOException {
		List<Integer> list = new ArrayList<Integer>();
		for (String s : Arrays.asList(readFile(file).split(","))) {
			list.add(Integer.valueOf(s.trim()));
		}
		return list;
	}

	private static String readFile(File f) throws IOException {
		char buf[] = new char[(int) f.length()];
		new FileReader(f).read(buf);
		return new String(buf);
	}

	private static List<String> generatePasswords() {
		List<String> passwords = new ArrayList<String>();

		for (int i = 'a'; i <= 'z'; i++)
			for (int j = 'a'; j <= 'z'; j++)
				for (int k = 'a'; k <= 'z'; k++)
					passwords.add(String.format("%c%c%c", i, j, k));

		return passwords;
	}

	private static String decipher(List<Integer> ciphertext, String password) {
		StringBuilder s = new StringBuilder(ciphertext.size());
		int i = 0;
		for (int c : ciphertext) {
			s.append((char) (c ^ password.charAt(i++ % password.length())));
		}
		return s.toString();
	}

	private static Map<String, Boolean> createWordMap(String text) {
		Map<String, Boolean> map = new HashMap<String, Boolean>();
		for (String s : text.split("\\s")) {
			if (s.length() > 4)
				map.put(s.toLowerCase(), Boolean.TRUE);
		}
		return map;
	}

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
		List<Integer> list = readCipherFile(new File("cipher1.txt"));
		Map<String, Boolean> wordMap = createWordMap(readFile(new File(
				"english.words")));

		int max = 0;
		String result = null;
		for (String password : generatePasswords()) {
			String cleartext = decipher(list, password);
			int n = 0;
			for (String w : cleartext.split("\\s")) {
				if (wordMap.containsKey(w.toLowerCase())) {
					n++;
				}
			}

			if (n > max) {
				System.out.println("Clear text: " + cleartext);
				System.out.println("Password: " + password);
				max = n;
				result = cleartext;
			}
		}

		int answer = 0;
		for (char c : result.toCharArray()) {
			answer += c;
		}

		System.out.println("Answer: " + answer);
	}
}
