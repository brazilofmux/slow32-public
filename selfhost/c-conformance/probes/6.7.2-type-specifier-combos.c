// 6.7.2p2  the permitted type-specifier multisets, in their usual order
#include <stdio.h>
int main(void) {
    signed char a = -1; unsigned char b = 255; short int c = -2; unsigned short d = 65535;
    signed int e = -3; unsigned f = 4000000000u; long signed int g = -5; long unsigned h = 6;
    long long int i = -7; unsigned long long int j = 18446744073709551615ull;
    char m = 'A'; _Bool n = 1;
    printf("%d %d %d %d %d %u %ld %lu %lld %llu %c %d\n", a, b, c, d, e, f, g, h, i, j, m, n);
    printf("%u %u %u %u %u %u\n", (unsigned)sizeof(short), (unsigned)sizeof(long),
           (unsigned)sizeof(long long), (unsigned)sizeof(_Bool), (unsigned)sizeof(long double),
           (unsigned)sizeof(float));
    return 0;
}
