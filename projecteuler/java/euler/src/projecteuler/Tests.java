package projecteuler;

import java.util.ArrayList;

import org.junit.Assert;
import org.junit.Test;

public class Tests {

	@Test
	public void testPrimeSieve() {
		ArrayList<Integer> sieve = PrimeSieve.sieve(2000);
		Assert.assertEquals(303, sieve.size());
	}

	@Test
	public void testPrimeSieveLarge() {
		ArrayList<Integer> sieve = PrimeSieve.sieve(1000000);
		Assert.assertEquals(78498, sieve.size());
	}

	@Test
	public void testDivisors() {
		ArrayList<Long> divisors = Divisors.getDivisors(28);
		Assert.assertEquals(5, divisors.size());
	}

	@Test
	public void testDivisorSum() {
		long sum = Divisors.getDivisorSum(28);
		Assert.assertEquals(28, sum);
	}

	@Test
	public void testIsPerfect() {
		Assert.assertTrue(Divisors.isPerfect(28));
	}

	@Test
	public void testProblem35() {
		ArrayList<Integer> list = EulerProblems.problem35();
		Assert.assertEquals(1457, list.size());
	}
}
