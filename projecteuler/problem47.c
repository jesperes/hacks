#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int num_distinct_pf(uint64_t n)
{
  int numdiv = 0;
  
  for (uint64_t p = 2; p <= n; p++) {
    if (n % p == 0) {
      numdiv++;
      while (n % p == 0)
        n = n / p;
    }
  }
  
  return numdiv;
}

int main(int argc, char *argv[])
{
  int num_cons = 0;
  int limit = 4;
  
  assert(num_distinct_pf(644) == 3);
  assert(num_distinct_pf(645) == 3);
  assert(num_distinct_pf(646) == 3);
  
  for (uint64_t n = 1; ; n++) {
    int npf = num_distinct_pf(n);
    // printf("npf(%lld) = %d\n", n, npf);

    if (npf == limit) {
      num_cons++;
      if (num_cons == limit) {
        printf("n = %lld\n", n - limit + 1);
        exit(0);
      }
    } else {
      num_cons = 0;
    }
  }
}
