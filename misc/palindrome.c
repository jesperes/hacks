#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

int palindrome(const char *s, int len)
{
  while (len > 1)
    if (s[0] == s[len-1])
      s++, len -= 2;
    else
      return 0;
  
  return 1;
}

int main(int argc, char *argv[])
{
  int size = atoi(argv[1]);
  char *s = malloc(size);
  for (int i = 0; i < size; i++) {
    s[i] = 'A';
  }
  s[size - 1] = 0;
  
  printf("Checking string of length %d\n", size);
  
  clock_t t0 = clock();

  if (palindrome(s, size-1))
    printf("palindrome!\n");
  else
    printf("not palindrome!\n");

  t0 = clock() - t0;
  printf("Time taken: %g seconds\n", (double)t0 / CLOCKS_PER_SEC);
  return 0;
}
