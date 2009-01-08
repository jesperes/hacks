#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

void
sieve(int *s, int n, int print_primes)
{
  int nexti = 2;                /* index of next prime */
  int p;
  int nump = 0;                 /* number of primes */

  for (int i = 0; i < n; i++) {
    s[i] = i;
  }

  while (1) {
    while ((nexti < n) && (p = s[nexti]) == 0) {
      nexti++;
    }

    if (nexti >= n)
      break;

    // p is now the next prime
    assert(p != 0);

    // increment number of primes found
    nump++;

    if (print_primes)
      printf("%d\n", p);

    // Cross out all multiples of p
    for (int i = nexti + p; i < n; i += p) {
      s[i] = 0;
    }

    nexti++;
  }
}

int main(int argc, char *argv[])
{
  int max = atol(argv[1]);
  int *s = malloc(max * sizeof(int));
  sieve(s, max, 1);
  free(s);
  return 0;
}
