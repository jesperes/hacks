#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LIMIT 10000

int sumdiv[LIMIT];

int
sum_divisors(int n)
{
  int sum = 0;

  if (sumdiv[n] > 0)
    return sumdiv[n];

  for (int i = 1; i < n; i++) {
    if (n % i == 0)
      sum += i;
  }

  sumdiv[n] = sum;
  return sum;
}

int main(int argc, char *argv[])
{
  int sum = 0;
  memset(sumdiv, 0, LIMIT);

  for (int i = 1; i < 10000; i++) {
    int a = sum_divisors(i);
    if (a >= 10000)
      continue;
    
    int b = sum_divisors(a);
    if (b == i && b != a) {
      printf("amicable pair: %d, %d\n", a, b);
      sum += i;
    }
  }
  
  printf("sum = %d\n", sum);
}
