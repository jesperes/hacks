package se.eskilson.alfapet;

import java.util.HashMap;
import java.util.Map;

public class LetterPoints {
	static private Map<Character, Integer> map = new HashMap<Character, Integer>();

	static void put(char c, int i) {
		map.put(Character.valueOf(c), i);
	}

	static {
		put('A', 1);
		put('B', 1);
		put('C', 1);
		put('D', 1);
		put('E', 1);
		put('F', 1);
		put('G', 1);
		put('H', 1);
		put('I', 1);
		put('J', 1);
		put('K', 1);
		put('L', 1);
		put('M', 1);
		put('N', 1);
		put('O', 1);
		put('P', 1);
		put('Q', 1);
		put('R', 1);
		put('S', 1);
		put('T', 1);
		put('U', 1);
		put('V', 1);
		put('W', 1);
		put('X', 1);
		put('Y', 1);
		put('Z', 1);
		put(' ', 0);
	}

	static int valueOf(char c) {
		return map.get(Character.toUpperCase(c));
	}
}
