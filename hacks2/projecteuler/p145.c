
#include <stdio.h>

typedef unsigned long ulong;

#define LEN (10)
#define MAX (1000000000)

int main() {
  int bcd[ LEN ] = { 1 }, l = 1; // Length in digits
  ulong n, s = 0;

  for( n = 1; n < MAX; ++n ) {
    int i;
    if( bcd[ 0 ] ) { // No trailing zeroes
      int c = 0; // Carry
      for( i = 0; i < l; ++i ) {
        int d = bcd[ i ] + bcd[ l-1-i ] + c; // Digits of sum
        if( !( d & 1 ) ) break; // Want odd ones
        c = d >= 10; // New carry
      }
      if( i == l ) ++s; // Got one
    }
// Increment
    for( i = 0; bcd[ i ] == 9; ++i ) bcd[ i ] = 0;
    if( i == l ) { l++; bcd[ i ] = 1; } else bcd[ i ]++;
  }
  printf( "%lu\n", s ); // Result

  return 0;
} 
