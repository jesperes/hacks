package projecteuler;

import java.util.ArrayList;

public class EulerProblems {

	public static ArrayList<Integer> problem35() {
		final int limit = 28123;
		ArrayList<Integer> abundantNumbers = new ArrayList<Integer>();
		ArrayList<Integer> nonSums = new ArrayList<Integer>();

		for (int i = 3; i < limit; i++) {
			if (Divisors.isAbundant(i)) {
				abundantNumbers.add(i);
			}
		}

		boolean[] sums = new boolean[limit];

		for (int a1 : abundantNumbers) {
			for (int a2 : abundantNumbers) {
				int sum = a1 + a2;
				if (sum < sums.length) {
					sums[sum] = true;
				}
			}
		}

		for (int i = 0; i < sums.length; i++) {
			if (!sums[i]) {
				nonSums.add(i);
			}
		}

		return nonSums;
	}
}
