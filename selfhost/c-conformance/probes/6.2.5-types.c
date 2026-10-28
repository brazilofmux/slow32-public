// 6.2.5p5-p8  standard integer type ranges nest; unsigned arithmetic wraps modulo 2^N
#include <stdio.h>
#include <limits.h>
int main(void) {
    unsigned u = UINT_MAX; u += 2;
    unsigned char uc = 255; uc++;
    unsigned short us = 0; us--;
    unsigned long long ull = 0; ull -= 1;
    int ok = sizeof(char) == 1 && sizeof(short) <= sizeof(int) && sizeof(int) <= sizeof(long)
          && sizeof(long) <= sizeof(long long) && SCHAR_MAX <= SHRT_MAX && LLONG_MAX >= LONG_MAX
          && sizeof(_Bool) >= 1 && sizeof(unsigned) == sizeof(int);
    printf("%u %d %d %llu %d\n", u, uc, us, ull, ok);
    return 0;
}
