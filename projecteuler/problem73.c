#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

#define ARRAYSIZE 1000000

int main()
{
  mpq_t frac;
  mpq_t lower, upper;
  mpq_t *fracs = malloc(ARRAYSIZE * sizeof(mpq_t));

  mpq_init(frac);
  mpq_init(lower);
  mpq_init(upper);
  
  for(int i = 0; i < ARRAYSIZE; i++) {
    mpq_init(fracs[i]);
  }
  
  mpq_set_ui(lower, 1, 3);
  mpq_set_ui(upper, 1, 2);
  
  int64_t count = 0;

  puts("Done initializing.");

  for (int d = 1; d < 10000; d++) {
    for (int n = d/3; n <= d/2; n++) {
      mpq_set_ui(frac, n, d);
      mpq_canonicalize(frac);

      printf("Checking %d/%d = %g\n", n, d, mpq_get_d(frac));
      
      if (mpq_cmp(frac, lower) > 0 && mpq_cmp(frac, upper) < 0) {
        int unique = 1;
        for (int i = 0; i < count; i++) {
          int c = mpq_cmp(frac, fracs[i]);
          if (c == 0) {
            unique = 0;
            break;
          }
        }
        
        if (unique) {
          mpq_set(fracs[count++], frac);
        }
      }
    }
  }
  
  printf("count = %lld\n", count);
}
