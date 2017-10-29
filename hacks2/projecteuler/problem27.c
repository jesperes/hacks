#include <stdio.h>
#include <stdarg.h>
#include <gmp.h>

int main(int argc, char *argv[])
{
  mpq_t n, p, t;
  mpq_init(n);
  mpq_init(p);
  mpq_init(t);
  mpq_set_str(t, "10000000000000000000000000", 10);
  for (int i = 1; i < 10; i++) {
    mpq_set_ui(n, 1, i);
    mpq_mul(p, n, t);
    mpq_out_str(stdout, 10, p);
    printf("\n");
  }
  mpq_clear(n);
  mpq_clear(p);
  mpq_clear(t);
}
