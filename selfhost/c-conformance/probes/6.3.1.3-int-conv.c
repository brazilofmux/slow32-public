// 6.3.1.3p1-p2  value-preserving conversions; to unsigned reduces modulo 2^N
#include <stdio.h>
int main(void) {
    int n = -1;
    long long big = 0x123456789LL;
    unsigned u = (unsigned)n;
    unsigned char uc = (unsigned char)-3;
    unsigned short us = (unsigned short)70000;
    unsigned v = (unsigned)big;
    unsigned long long ull = (unsigned long long)n;   /* sign-extend then modulo */
    long long sx = (long long)n;
    long long zx = (long long)u;
    printf("%u %d %d %u %llu %lld %lld\n", u, uc, us, v, ull, sx, zx);
    return 0;
}
