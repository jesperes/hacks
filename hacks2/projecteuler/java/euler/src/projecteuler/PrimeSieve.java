package projecteuler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PrimeSieve {

    static private boolean[] sieve0(int max) {

        // Array of booleans. Indexes set to true are *not* prime.
        boolean[] sieve = new boolean[max + 1];
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

        return sieve;
    }

    public static List<Integer> sieve(int max) {
        boolean[] sieve = sieve0(max);
        List<Integer> primes = new ArrayList<Integer>();
        for (int i = 0; i < sieve.length; i++) {
            if (!sieve[i]) {
                primes.add(i);
            }
        }
        return primes;
    }

    public static Map<Integer, Boolean> sieve_map(int max) {
        boolean[] sieve = sieve0(max);
        Map<Integer, Boolean> primes = new HashMap<Integer, Boolean>();
        for (int i = 0; i < sieve.length; i++) {
            if (!sieve[i]) {
                primes.put(i, Boolean.TRUE);
            }
        }
        return primes;

    }
}
