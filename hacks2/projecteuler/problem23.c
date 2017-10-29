#include <stdio.h>
#define DIVISOR_LIMIT 512
#define ABUNDANT_LIMIT 28123
/*
  The largest integer which cannot be written as a sum of two abundant numbers
  is less than 28123.
  
 */

int divisors[DIVISOR_LIMIT] = {0};
int abundant_numbers[ABUNDANT_LIMIT] = {0};
int n_abundant = 0;

int is_abundant(int n)
{
  int sum = 0;
  for (int i = 1; i < n; i++) {
    if (n % i == 0)
      sum += i;
  }

  return (sum > n) ? 1 : 0;
}

#define min(x,y) ((x) < (y) ? (x) : (y))

int is_abundant_sum(int n)
{
  int lim = min(n, n_abundant);
  
  for (int ai = 0; ai < lim; ai++) {
    for (int bi = 0; bi < lim; bi++) {
      if (ai + bi == n) {
        return 1;
      }
    }
  }

  return 0;
}

int main()
{
  int n = 0;
  for (int i = 1; i < 28123; i++) {
    if (is_abundant(i)) {
      abundant_numbers[n_abundant++] = i;
    }
  }
  printf("Abundant numbers < 28123: %d\n", n_abundant);

  int sum = 0;
  for (int i = 1; i < 28123; i++) {
    if (!is_abundant_sum(i)) {
      printf("Not abundant sum: %d\n", i);
      sum += i;
    } else {
      printf("Abundant sum: %d\n", i);
    }
  }

  printf("Sum of all n such that !is_abundant_sum(n) == %d\n", sum);
  return n;
}

