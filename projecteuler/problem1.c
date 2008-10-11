#include <stdio.h>

int sum_of_divisibles(int n)
{
  if (n == 0)
    return 0;

  if (n % 3 == 0 || n % 5 == 0)
    return n + sum_of_divisibles(n-1);
  else
    return sum_of_divisibles(n-1);
}

int main()
{
  printf("%d\n", sum_of_divisibles(999));
  return 0;
}
 
