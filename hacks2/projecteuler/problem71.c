#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>

int main()
{
  mpq_t frac;
  mpq_t limit, max;

  mpq_init(frac);
  mpq_init(limit);
  mpq_init(max);
  
  mpq_set_ui(limit, 3, 7);
  mpq_set_ui(max, 0, 1);

  printf("3/7 == %g\n", mpq_get_d(limit));

  for (int d = 990000; d < 1000000; d++) {
    for (int n = (int)(427*d/1000); n < d; n++) {
      mpq_set_ui(frac, n, d);
      mpq_canonicalize(frac);
       
      // printf("checking %g\n", mpq_get_d(frac));

      // if frac is less than limit (3/7), but larger than max,
      // set max to frac.
      if (mpq_cmp(frac, limit) < 0) {
        if (mpq_cmp(frac, max) > 0)
          mpq_set(max, frac);
      } else {
        // We're equal to 3/7 or above. Break, and start over with the
        // next denominator.
        break;
      }
    }
  }
  
  mpq_out_str(stdout, 10, limit);
  puts("");
  mpq_out_str(stdout, 10, max);
  puts("");
  printf("%g\n", mpq_get_d(max));
}
