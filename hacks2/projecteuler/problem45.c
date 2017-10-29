#include <stdio.h>
#include <assert.h>

#define LIMIT 100000

int tlist[LIMIT] = {0};
int plist[LIMIT] = {0};
int hlist[LIMIT] = {0};

int main()
{
  for (int i = 0; i < LIMIT; i++) {
    tlist[i] = i*(i+1)/2;
    plist[i] = i*(3*i-1)/2;
    hlist[i] = i*(2*i-1);
  }

  assert(tlist[285] == 40755);
  assert(plist[165] == 40755);
  assert(hlist[143] == 40755);

  printf("Init done.\n");
  for (int h = 144; h < LIMIT; h++) {
    for (int p = 166; p < LIMIT && plist[p] <= hlist[h]; p++) {
      
      if (plist[p] == hlist[h]) {
        printf("Entering inner loop; P%d == H%d == %d\n", p, h, plist[p]);

        for (int t = 286; t < LIMIT; t++) {
          if (tlist[t] > plist[p])
            break;
          
          if (tlist[t] == plist[p]) {
            printf("Found number: %d\n", tlist[t]);
            return 0;
          }
        }

        printf("No triangle number matching %d\n", plist[p]);
      }
    }
  }
}
