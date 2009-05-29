#include <stdio.h>

int main()
{
  int sumsq = 0;
  int sqsum = 0;

  for (int i = 1; i <= 100; i++) {
    sumsq += i*i;
    sqsum += i;
  }

  sqsum *= sqsum;
  printf("diff = %d\n", sqsum - sumsq);
}
