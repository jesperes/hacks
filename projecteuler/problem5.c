#include <stdio.h>
#include <stdlib.h>

int divisible_upto(int n, int upto) 
{
  for (int i = 1; i <= upto; i++) {
    if (n % i != 0)
      return 0;
  }
  return 1;
}

int main()
{
  int n;

  for (n = 10000; ; n++) {
    if (divisible_upto(n, 20))
      break;
  }

  printf("n = %d\n", n);
}
