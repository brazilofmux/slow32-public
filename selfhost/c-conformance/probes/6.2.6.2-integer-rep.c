// 6.2.6.2p2  signed integers: two's complement here; conversions to unsigned preserve the bit pattern
#include <stdio.h>
#include <limits.h>
int main(void) {
    int m = -1;
    long long n = -2;
    signed char sc = -128;
    printf("%x %llx %x %d %d\n", (unsigned)m, (unsigned long long)n, (unsigned char)sc,
           INT_MIN + INT_MAX, (int)(-5 & 0xff));
    return 0;
}
