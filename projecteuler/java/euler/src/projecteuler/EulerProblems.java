package projecteuler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class EulerProblems {

	public static int problem35() {
		final int limit = 20161;
		ArrayList<Integer> abundantNumbers = new ArrayList<Integer>();

		for (int i = 3; i < limit; i++) {
			if (Divisors.isAbundant(i)) {
				abundantNumbers.add(i);
			}
		}

		boolean[] sums = new boolean[limit + 1];

		for (int a1 : abundantNumbers) {
			for (int a2 : abundantNumbers) {
				int n = a1 + a2;
				if (a1 <= a2 && n < sums.length) {
					sums[n] = true;
				}
			}
		}

		int sum = 0;
		for (int i = 1; i < sums.length; i++) {
			if (!sums[i]) {
				sum += i;
			}
		}

		return sum;
	}

	public static int alphaValue(String s) {
		int sum = 0;
		for (byte c : s.getBytes()) {
			sum += (c - 65 + 1);
		}
		return sum;
	}

	public static int problem42() throws IOException {
		int numTriangleWords = 0;
		Collection<Integer> list = Utils.triangleNumbers(20000);
		for (String s : Files.read("words.txt").split(",")) {
			int alpha = alphaValue(s.trim().substring(1, s.length() - 1));
			if (list.contains(alpha))
				numTriangleWords++;
		}
		return numTriangleWords;
	}

	public static int problem52(int lim) {
		for (int n = 1; true; n++) {
			byte[] b = String.valueOf(n).getBytes();
			Arrays.sort(b);
			boolean isPerm = true;

			for (int i = lim; i >= 2 && isPerm; i--) {
				int x = i * n;
				byte[] a = String.valueOf(x).getBytes();
				Arrays.sort(a);

				if (a.length != b.length)
					isPerm = false;
				else {
					for (int j = 0; j < a.length && isPerm; j++) {
						isPerm = (a[j] == b[j]);
					}
				}
			}

			if (isPerm)
				return n;
		}
	}
}
