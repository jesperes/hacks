#include <stdio.h>
#include <math.h>
#include <stdint.h>

#define NUMBER 600851475143ULL

int main()  
{
  int64_t n = NUMBER;
  int64_t acc = 1;
  int i;

  for (i = 2; acc < NUMBER; i++) {
    if (n % i == 0) {
      n = n / i;
      acc *= i;
    }
  }
  printf("Largest factor: %d\n", i);
}
