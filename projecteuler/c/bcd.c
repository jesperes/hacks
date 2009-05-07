
typedef struct {
  int digits[20];
  int len;
} bcd_t;

bool
is_pandigital(bcd_t *bcd, int n)
{
  int flags[10] = {0};
  for (int i = 0; i < bcd->len; i++) {
    flags[bcd->digits[i]]++;
  }

  for (int i = 1; i <= n; i++) {
    if (flags[i] != 1)
      return false;
  }

  return true;
}

void
bcd_incr(bcd_t *bcd) {
  int i;
  for (i = 0; bcd->digits[i] == 9; i++) {
    bcd->digits[i] = 0;
  }
  
  if (i == bcd->len) {
    bcd->digits[i] = 1;
    bcd->len++;
  } else {
    bcd->digits[i]++;
  }
}

int
bcd_to_int(bcd_t *bcd) {
  int n = 0;
  for (int i = 0, e = 1; i < bcd->len; i++, e *= 10) {
    n += bcd->digits[i] * e;
  }
  return n;
}

void
int_to_bcd(bcd_t *bcd, int n)
{
  bcd->len = 0;
  for (int i = 0; n > 0; i++) {
    bcd->digits[i] = n % 10;
    n /= 10;
    bcd->len++;
  }
}

