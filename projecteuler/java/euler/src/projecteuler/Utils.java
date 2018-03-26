package projecteuler;

import java.util.ArrayList;
import java.util.Collection;

public class Utils {
	public static Collection<Integer> triangleNumbers(int limit) {
		ArrayList<Integer> list = new ArrayList<Integer>();
		for (int n = 1; n < limit; n++) {
			int tn = (n * (n + 1)) / 2;
			list.add(tn);
		}
		return list;
	}
}
