#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <ctime>

using namespace std;

static clock_t t0 = 0;
static char buf[20];

inline void to_str(int i)
{
  sprintf(buf, "%d", i);
}

static char rbuf[20];

int
main() 
{
  int totalsum = 0;
  t0 = clock();
  
  for (int n = 1; n < 1000000; n++) {
    if (n % 10 == 0)
      continue;
    
    to_str(n);
    int len = strlen(buf); 
    
    // If first and last digits are both odd or both even, then n +
    // reverse(n) will be even, so we can return early.
    if ((buf[0] % 2) == (buf[len-1] % 2))
      continue;
    
    for (int i = 0; i < len; i++) {
      rbuf[len - i - 1] = buf[i];
    } 
    
    rbuf[len] = 0;
    int sum = n + atol(rbuf);  
    
    to_str(sum);
    len = strlen(buf);
    for (int i = 0; i < len; i++) {
      if (buf[i] % 2 == 0) {
        goto skip;
      }
    }

    totalsum += sum;
  skip:
    continue;
  }
  
  t0 = (clock() - t0);
  
  cout << "Reversible numbers: " << totalsum << endl;
  cout << "Time: " << ((double)t0 / CLOCKS_PER_SEC) << endl;
}
