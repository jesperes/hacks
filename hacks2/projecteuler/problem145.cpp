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
static char rbuf[20];
const int LIMIT = 1000000000;

int
main() 
{
  int num_reversibles = 0;
  t0 = clock();
  
  bool first_digit_even = false;
  int nlen = 2;                 // length of n
  int nmag = 10;                // current magnitude of n
  int nnext = 100;              // when is next increase in nlen?

  for (int n = 10; n < LIMIT; n++) {
#if 0
    cout << "n = " << n << endl;
    cout << "nmag = " << nmag << endl;
    cout << "nnext = " << nnext << endl;
    cout << "nlen = " << nlen << endl;
#endif

    if (n == nnext) {
      // We've just jumped from 9... to 10...
      nlen++;
      nmag *= 10;
      nnext *= 10;
      first_digit_even = false;

      // ...but reverse(n) will have a leading zero, so skip it.
      continue;

    } else if (n % nmag == 0) {
      // When n % nmag == 0, the left-most digit has just shifted from
      // even to odd (or vice versa).
      // cout << "First digit switching evenness: " << n << endl;
      first_digit_even = ((n / nmag) % 2 == 0);
      continue;
    }
    
    if (n % 10 == 0)
      continue;
    
    bool last_digit_even = (n % 2 == 0);
    if (first_digit_even == last_digit_even)
      continue;

    sprintf(buf, "%d", n);
    for (int i = 0, revi = nlen - 1; revi >= i; i++, revi--) {
      int revi = nlen - i - 1;
      rbuf[i] = buf[revi];
      rbuf[revi] = buf[i];
    }
    
    // cout << "buf = " << buf << ", rbuf = " << rbuf << endl;
    
    int revsum = n + atol(rbuf);
    int all_odd = true;
    while (revsum > 0) {
      if ((revsum % 2) == 0) {
        all_odd = false;
        break;
      }
      
      revsum = revsum / 10;
    }
    
    num_reversibles += all_odd;
  }
  
  t0 = (clock() - t0);
  
  cout << "Reversible numbers: " << num_reversibles << endl;
  cout << "Time per " << LIMIT << " iterations: " << 
    ((double)t0 / CLOCKS_PER_SEC) << " seconds " << endl;
}
