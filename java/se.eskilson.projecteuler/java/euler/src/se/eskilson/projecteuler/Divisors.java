package se.eskilson.projecteuler;

import java.util.ArrayList;
import java.util.Collections;

public class Divisors {
	// Return the sum of the divisors of n
	public static long getDivisorSum(long n) {
		long sum = 0;
		double sqrt = Math.sqrt(n);

		for (long i = 1; i <= sqrt; i++) {
			if (i == sqrt) {
				sum += i;
			} else {
				if (n % i == 0) {
					sum += i;
					if (i > 1) {
						// only include proper divisors
						sum += n / i;
					}
				}
			}
		}
		return sum;
	}

	public static long getNumDivisors(long n) {
		long num = 0;
		double sqrt = Math.sqrt(n);

		for (long i = 1; i <= sqrt; i++) {
			if (i == sqrt) {
				num++;
			} else {
				if (n % i == 0) {
					if (i > 1) {
						num += 2;
					} else {
						num++;
					}
				}
			}
		}
		return num;
	}

	public static ArrayList<Long> getDivisors(long n) {
		ArrayList<Long> divisors = new ArrayList<Long>();
		double sqrt = Math.sqrt(n);

		for (long i = 1; i <= sqrt; i++) {
			if (i == sqrt) {
				divisors.add(i);
			} else {
				if (n % i == 0) {
					divisors.add(i);
					if (i > 1) {
						// only include proper divisors
						divisors.add(n / i);
					}
				}
			}
		}
		Collections.sort(divisors);
		return divisors;
	}

	public static boolean isAbundant(long n) {
		return getDivisorSum(n) > n;
	}

	public static boolean isDeficit(long n) {
		return getDivisorSum(n) < n;
	}

	public static boolean isPerfect(long n) {
		return getDivisorSum(n) == n;
	}
}
