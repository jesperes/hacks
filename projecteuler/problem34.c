#include <stdio.h>
#include <limits.h>
#include <time.h>
#include <stdlib.h>

#ifdef USE_INT
typedef int number_t;
#define LIMIT INT_MAX
#define FMT "%d"
#else
typedef long long number_t;
#define LIMIT LLONG_MAX
#define FMT "%lld"
#endif

#define FACTABLE_SIZE 16 * 1024
static number_t factable[FACTABLE_SIZE];
static int factable_initialized = 0;

number_t 
fac(number_t n)
{
  if (factable_initialized && n < FACTABLE_SIZE)
    return factable[n];
  
  if (n <= 1)
    return n;
 
  number_t f = n * fac(n-1);
  if (f > LLONG_MAX/2) {
    printf("Bork! f = %lld", f);
    exit(1);
  }

  return f;
}

void
initfactable()
{
  printf("Initializing factorial table... ");
  fflush(stdout);

  for (int i = 0; i < FACTABLE_SIZE; i++) {
    factable[i] = fac(i);
  }
  factable_initialized = 1;

  printf("done.\n");
}

static number_t digits[100];
static number_t ndigits;

void
get_digits(number_t n)
{
  ndigits = 0;
  for (number_t i = 0; n > 0; i++) {
    digits[i] = n % 10;
    n = n / 10;
    ndigits++;
  }
}

number_t
facsum(number_t n)
{
  number_t sum = 0;
  get_digits(n);
  for (number_t i = 0; i < ndigits; i++) {
    sum += fac(digits[i]);
  }
  return sum;
}

int 
main(int argc, char *argv[])
{
  initfactable();

  clock_t t0 = clock();

  for (number_t i = 3; i < LIMIT; i++) {
    if (i % 10000000 == 0) {
      clock_t t1 = clock();
      double secs = ((double)(t1 - t0))/CLOCKS_PER_SEC;
      printf("At i = " FMT " (%g secs)\n", i, secs);
      t0 = t1;
    }
    
    if (facsum(i) == i) {
      printf(FMT "\n", i);
    }
  }
}
