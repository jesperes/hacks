/*
 * Lisp parser.
 */

#include <stdio.h>
#include <ctype.h>

int next_token() 
{
  int c = getchar();
  if (isspace(c)) {
    return next_token();
  }

  return c;
}

int main(int argc, char *argv[]) 
{
  int c = 0;
  while ((c = next_token()) != EOF) {
    printf("Char %d\n", c);
  }

  return 0;
}
