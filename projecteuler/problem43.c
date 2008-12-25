#include <stdio.h>

int digits[10];

int *get_digits(int n)
{
  for (int i = 0; n > 0; i++) {
    digits[i] = n % 10;
    n /= 10;
  }
}

int main()
{

}
