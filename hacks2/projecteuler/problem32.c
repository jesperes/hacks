#include <assert.h>
#include <string.h>
#include <stdio.h>

int
has_dups_str(char *s)
{
  int len = strlen(s);
  int digits[10] = { 0 };
  for (int i = 0; i < len; i++) {
    int c = s[i] - '0';
    if (digits[c] > 0)
      return 1;
    else
      digits[c]++;
  }
  return 0;
}

int
has_dups(int n)
{
  static char buf[256];
  sprintf(buf, "%d", n);
  return has_dups_str(buf);
}

int
has_dups2(int n, int m)
{
  static char buf[256];
  sprintf(buf, "%d%d", n, m);
  return has_dups_str(buf);
}

int
is_9_pandigital(int n, int m, int p)
{
  static char buf[256];
  sprintf(buf, "%d%d%d", n, m, p);
  int len = strlen(buf);
  if (len != 9)
    return 0;

  int digits[10] = { 0 };
  for (int i = 0; i < len; i++) {
    int c = buf[i] - '0';
    if (c == 0)
      return 0;

    digits[c]++;
  }

  for (int i = 1; i < len; i++) {
    int c = buf[i] - '0';
    if (digits[c] != 1) {
      return 0;
    }
  }

  return 1;
}

int main()
{
  assert(is_9_pandigital(12, 345, 6789));
  assert(!is_9_pandigital(11, 345, 6789));
  
  for (int i = 1; i < 9999; i++) {
    if (has_dups(i))
      continue;
    
    for (int j = i+1; j < 9999; j++) {
      if (has_dups2(i, j))
        continue;
      
      int k = i * j;
      if (is_9_pandigital(i, j, k)) {
        printf("Pandigital product: %d * %d == %d\n", i, j, k);
      }
    }
  }
}
