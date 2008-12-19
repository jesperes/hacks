#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

inline int bit_get(int32_t *bitarray, int64_t i) {
  return !!(bitarray[i >> 5] & (1 << (i % 32)));
}

inline void bit_set(int32_t *bitarray, int64_t i) {
  bitarray[i >> 5] |= (1 << (i % 32));
}

inline void bit_clear(int32_t *bitarray, int64_t i) {
  bitarray[i >> 5] &= ~(1 << (i % 32));
}

inline int32_t *alloc_bits(int64_t size) {
  return malloc(size >> 3);
}


int32_t *sieve(int max)
{
  int32_t *sieve = malloc(sizeof(int32_t) * max);

  for (int i = 0; i < 1000; i++) {
    bit_set(sieve, i);
    assert(bit_get(sieve, i) == 1);
  }

  for (int i = 0; i < 1000; i++) {
    bit_clear(sieve, i);
    assert(bit_get(sieve, i) == 0);
  }

  printf("0x%08lx\n", sieve[0]);

  return sieve;
}

int main()
{
  sieve(100);
  return 0;
}
