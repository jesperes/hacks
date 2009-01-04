/* Generic C program */

#include <stdio.h>
#include <stdlib.h>


int main(int argc, char *argv[])
{

  int n = 76376;
  int turn = (((n & -n) << 1) & n) != 0;
  printf("turn = %s\n", turn ? "right" : "left");
  return 0;
}
