#include <stdio.h>

void digits(int n, int *array)
{
  for (int i = 0; n > 0; i++) {
    array[i] = n % 10;
    n /= 10;
  }
}

void loop()
{
  int d[10];
  
  /*
   * d_(2)d_(3)d_(4)=406 is divisible by 2
   * d_(3)d_(4)d_(5)=063 is divisible by 3
   * d_(4)d_(5)d_(6)=635 is divisible by 5
   * d_(5)d_(6)d_(7)=357 is divisible by 7
   * d_(6)d_(7)d_(8)=572 is divisible by 11
   * d_(7)d_(8)d_(9)=728 is divisible by 13
   * d_(8)d_(9)d_(10)=289 is divisible by 17
   *
   * d_6 => 0 or 5
   * d_4 => 0, 2, 4, 6, 8
   * d_3 + d_4 + d_5 => divisible by 3
   */

  
}

int main(int argc, char *argv[])
{
  int d[10];
  digits(12345, d);
  for (int i = 0; i < 5; i++) {
    printf("%d, ", d[i]);
  }
  printf("\n");
}
