package projecteuler;

import java.util.ArrayList;

public class PrimeSieve {

	static ArrayList<Integer> sieve(int max) {
		
		// Array of booleans. Indexes set to true are *not* prime.
		boolean[] sieve = new boolean[max+1];
		int nextPrime = 0;
		
		sieve[0] = true;
		sieve[1] = true;
		sieve[3] = false;
		
		for (int i = 4; i < sieve.length; i += 2) {
			sieve[i] = true;
		}
		nextPrime = 3;
		do {
			while (sieve[nextPrime])
				nextPrime++;
			
			for (int i = nextPrime * 2; i < sieve.length; i += nextPrime) {
				sieve[i] = true;
			}
			
			nextPrime++;
		} while (nextPrime <= Math.sqrt(max));
		
		ArrayList<Integer> primes = new ArrayList<Integer>();
		for (int i = 0; i < sieve.length; i++) {
			if (!sieve[i]) {
				primes.add(i);
			}
		}
		return primes;
	}
}
