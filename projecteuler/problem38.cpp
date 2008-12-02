#include <iostream>
#include <cassert>

using namespace std;

bool
is_pandigital(int n)
{
  int digits[10] = {0};
  
  for (int n0 = n; n0 > 0; n0 /= 10) {
    digits[n0 % 10]++;
  }
  
  for (int i = 1; i <= 9; i++) {
    if (digits[i] != 1)
      return false;
  }

  return true;
}

int
main()
{
  int max = 0;

  assert(is_pandigital(123456789));

  for (int i = 192; i < 999999; i++) {
    char buf[20] = { 0 };
    int len = 0;

    if (i % 100000 == 0) {
      cout << "i = " << i << endl;
    }

    int pandigital = 0;
    int j;
    for (j = 1; true; j++) {
      sprintf(buf + len, "%d", j * i);
      // cout << "buf: " << buf << endl;

      int p = atol(buf);
      if (p >= 1000000000) {
        break;
      }
   
      pandigital = p;
      len = strlen(buf);
    }
   
    if (len == 9) {
      // cout << "Checking: " << i << ", " << len << ", " << pandigital << endl;
      
      if (is_pandigital(pandigital) and (pandigital > max)) {
        max = pandigital;
        cout << "New max: " << pandigital << " (" << i << ", " << j-1 << ")" << endl;
      }
    }
  }

  return 0;
}
