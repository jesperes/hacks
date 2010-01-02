package se.eskilson.projecteuler;


public class Problem92 {

	static long squareOfDigits(long n) {
		long sq = 0;
		for (char c : String.valueOf(n).toCharArray()) {
			int m = c - '0';
			sq += (m * m);
		}
		return sq;
	}

	static boolean endsWithEightyNine(long i) {
		while (i != 1 && i != 89) {
			i = squareOfDigits(i);
		}
		return i == 89;
	}

	public static void main(String[] args) throws InterruptedException {
		long count = 0;
		long start = System.nanoTime();
		for (long i = 1; i < 10000000; i++) {
			if (endsWithEightyNine(i))
				count++;
		}

		long elapsed = System.nanoTime() - start;
		double elapsedSecs = elapsed / 1000000000.0;
		System.out.println("Eighty-niners: " + count);
		System.out.println("Elapsed time: " + elapsedSecs);
	}
}
