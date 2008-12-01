package projecteuler;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

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
	public void testDigits() {
	}

	@Test
	public void testProblem35() {
		int sum = EulerProblems.problem35();
		Assert.assertEquals(4179871, sum);
	}

	@Test
	public void testProblem42() throws IOException {
		Collection<Integer> list = Utils.triangleNumbers(20);
		Assert.assertTrue(list.contains(28));
		Assert.assertEquals(19, EulerProblems.alphaValue("S"));
		Assert.assertEquals(8, EulerProblems.alphaValue("H"));
		Assert.assertEquals(55, EulerProblems.alphaValue("SKY"));

		int sum = EulerProblems.problem42();
		Assert.assertEquals(162, sum);
	}

	@Test
	public void testProblem52() {
		Assert.assertEquals(125874, EulerProblems.problem52(2));
		Assert.assertEquals(142857, EulerProblems.problem52(6));
	}

	@Test
	public void testProblem27() {
		Assert.assertEquals(-59231, EulerProblems.problem27());
	}

	@Test
	public void testProblem97() {
		// Assert.assertEquals(0, EulerProblems.problem97());
	}

}
