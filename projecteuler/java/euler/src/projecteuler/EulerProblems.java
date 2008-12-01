package projecteuler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

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

	public static int p27numPrimes(int a, int b,
			HashMap<Integer, Boolean> primeMap) {
		int n = 0;

		while (true) {
			int m = n * n + a * n + b;
			if (primeMap.containsKey(m))
				n++;
			else
				break;
		}
		return n;
	}

	public static int problem27() {
		List<Integer> primes = PrimeSieve.sieve(100000);
		HashMap<Integer, Boolean> primeMap = new HashMap<Integer, Boolean>();
		for (Integer p : primes) {
			primeMap.put(p, true);
		}

		int maxLength = 0;
		int max_a = 0, max_b = 0;

		for (int a = -999; a < 1000; a++) {
			for (int b = -999; b < 1000; b++) {
				int n = p27numPrimes(a, b, primeMap);
				if (n > maxLength) {
					maxLength = n;
					max_a = a;
					max_b = b;
				}
			}
		}

		return max_a * max_b;
	}

	private static boolean isPandigital(int i, int n) {
		String s = String.valueOf(i);
		int digits[] = new int[n];
		for (int j = 0; j < s.length(); j++) {
			digits[s.charAt(j) - '0']++;
		}

		for (int j = 0; i < digits.length; j++) {
			if (digits[j] != 1)
				return false;
		}

		return true;
	}

	private static int problem41() {
		List<Integer> primes = PrimeSieve.sieve(1000000000);
		long[] ndigit_pandigital_primes = new long[12];
		for (int p : primes) {
			int len = String.valueOf(p).length();
			if (isPandigital(p, len)) {
				long max = ndigit_pandigital_primes[len];
				if (p > max) {
					ndigit_pandigital_primes[len] = p;
				}
			}
		}

		for (long p : ndigit_pandigital_primes) {
			System.out.println("p = " + p);
		}
		return 0;
	}

	public static void main(String[] argv) {
		System.out.println(problem41());
	}
}
