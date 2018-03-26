#include <stdio.h>

static int sum = 1;
static int n = 1;

int get_next_triangle()
{
  int tn = sum;
  n++;
  sum += n;
  return tn;
}

int num_factors(int n)
{
  int m = 0;
  for (int i = 1; i <= n; i++) {
    if (n % i == 0)
      m++;
  }
  return m;
}

int main()
{
  int max = 0;
  while (max <= 500) {
    int n = get_next_triangle();
    if (n % 2 != 0) continue;
    int nf = num_factors(n);
    if (nf > max) {
      printf("%d -> %d\n", n, nf);
      max = nf;
    }
  }
  
  printf("max = %d\n", max);
}
