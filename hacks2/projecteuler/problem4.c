#include <stdio.h>
#include <string.h>

int is_palindrome(int n)
{
  char buf[20];
  sprintf(buf, "%d", n);
  for (int i = 0; i < strlen(buf)/2; i++) {
    if (buf[i] == buf[strlen(buf)-1])
      continue;
    else
      return 0;
  }
  
  return 1;
}

int main()  
{
  int maxp = 1;
  for (int x = 0; x < 1000; x++) {
    for (int y = 0; y < 1000; y++) {
      int n = x * y;
      if (is_palindrome(n) && n > maxp)
        maxp = n;
    }
  }

  printf("maxp = %d\n", maxp);
}
