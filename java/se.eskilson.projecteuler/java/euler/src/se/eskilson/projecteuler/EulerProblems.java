package se.eskilson.projecteuler;

import java.io.PrintStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EulerProblems {

    public static int alphaValue(String s) {
        int sum = 0;
        for (byte c : s.getBytes()) {
            sum += (c - 65 + 1);
        }
        return sum;
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

    private static boolean isPandigital(BigInteger i) {
        String s = i.toString();
        int n = s.length();
        int digits[] = new int[10];
        for (int j = 0; j < n; j++) {
            int d = s.charAt(j) - '0';
            digits[d]++;
            if (digits[d] >= 2)
                return false;
        }

        for (int j = 1; j <= n; j++) {
            if (digits[j] != 1)
                return false;
        }

        return true;
    }

    private static int problem41() {
        assert (isPandigital(BigInteger.valueOf(123456789)));
        assert (isPandigital(BigInteger.valueOf(12345)));

        BigInteger zero = BigInteger.ZERO;
        // BigInteger one = BigInteger.ONE;
        BigInteger two = BigInteger.valueOf(2);
        BigInteger three = BigInteger.valueOf(3);
        BigInteger five = BigInteger.valueOf(5);
        BigInteger seven = BigInteger.valueOf(7);

        int n = 0;
        for (BigInteger i = BigInteger.valueOf(987654321); i.compareTo(zero) > 0; i
                .subtract(two)) {
            n++;
            if (i.mod(three) == zero || i.mod(five) == zero
                    || i.mod(seven) == zero) {
                continue;
            }

            if (!isPandigital(i))
                continue;

            if (n % 100000 == 0) {
                System.out.println("Count = " + n);
            }

            // System.out.println("Checking pandigital " + i +
            // " for primeness");
            if (!i.isProbablePrime(20)) {
                continue;
            }

            System.out.println("Probable prime: " + i);
            return i.intValue();
        }

        return -1;
    }

    public static int problem97() {
        // This is just ridiculously slow, compared to GMP.
        return new BigInteger("2").pow(7830457).multiply(
                new BigInteger("28433")).add(BigInteger.ONE)
                .mod(BigInteger.TEN).intValue();
    }

    public static boolean isTruncatablePrime(int p, Map<Integer, Boolean> primes) {
        if (p <= 7)
            return false;

        if (!primes.containsKey(p))
            return false;

        String s = String.valueOf(p);
        boolean truncatable = true;
        for (int i = 1; i < s.length(); i++) {
            int sRight = Integer.valueOf(s.substring(i));
            if (!primes.containsKey(sRight)) {
                truncatable = false;
                break;
            }

            int sLeft = Integer.valueOf(s.substring(0, s.length() - i));
            if (!primes.containsKey(sLeft)) {
                truncatable = false;
                break;
            }
        }

        return truncatable;
    }

    // Truncatable primes
    public static int problem37() throws Exception {
        Map<Integer, Boolean> primes = PrimeSieve.sieve_map(1000000);
        int sum = 0;
        int count = 0;

        for (int p : primes.keySet()) {
            if (isTruncatablePrime(p, primes)) {
                sum += p;
                count++;
            }
        }

        if (count != 11)
            throw new Exception("count != 11");

        return sum;
    }

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

    public static void printList(PrintStream o, List<?> list) {
        o.print("[");
        String sep = "";
        for (Object elem : list) {
            o.print(sep);
            o.print(elem.toString());
            sep = ",";
        }
        o.println("]");
    }

    public static long sumList(List<? extends Number> list) {
        long sum = 0;
        for (Number n : list) {
            sum += n.intValue();
        }
        return sum;
    }

    public static long problem50() {
        int limit = 1000000;
        List<Integer> sieve = PrimeSieve.sieve(limit);
        Map<Integer, Boolean> sieveMap = PrimeSieve.sieve_map(limit);

        int numPrimes = sieve.size();
        int largestPrime = sieve.get(numPrimes - 1);

        System.out.println("Largest prime: " + largestPrime);

        for (int len = numPrimes; len > 0; len--) {
            /*
             * Check if there is a sequence of length "len" which adds up to a
             * prime. Loop over all possible sequences of length "len".
             */
            // System.out.println("Checking sequences of length " + len);
            long sum = 0;
            for (int i = 0; i + len <= numPrimes && sum < largestPrime; i++) {
                /*
                 * Check sequence [i..i+len].
                 */

                List<Integer> subList = sieve.subList(i, i + len);
                assert (subList.size() == len);

                // printList(System.out, subList);

                sum = sumList(subList);
                if (sum <= largestPrime) {
                    // System.out.println(String.format("%d..%d = %d", subList
                    // .get(0), subList.get(subList.size() - 1), sum));

                    if (sieveMap.containsKey(sum)) {
                        System.out.println(String.format(
                                "(%d..%d) == %d (%d elements)", subList.get(0),
                                subList.get(subList.size() - 1), sum, len));
                        return sum;
                    }
                }
            }
        }

        return -1;
    }

    public static void main(String[] argv) {
        System.out.println(problem50());
    }
}
