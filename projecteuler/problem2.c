#include <stdio.h>
int main()  
{
  int sum = 0;
  int x = 1;
  int y = 2;
  int tmp;
  while (y < 4000000) {
    if (y % 2 == 0) sum += y;
    tmp = x;
    x = y;
    y = tmp + y;
  }

  printf("%d\n", sum);
}
