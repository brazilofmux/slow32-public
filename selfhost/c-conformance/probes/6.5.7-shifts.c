// 6.5.7p3-5  shift values: promoted left operand, unsigned right shift is logical, 64-bit shifts
#include <stdio.h>
int main(void) {
    unsigned char uc = 0x80;
    volatile int n = 4;
    unsigned u = 0x80000000u;
    long long ll = 1;
    unsigned long long ull = 0x8000000000000000ULL;
    printf("%d %d\n", uc << 4, (int)sizeof(uc << 1));
    printf("%u %lld %llu\n", u >> 31, ll << 40, ull >> 63);
    printf("%d %d %lld\n", 1 << n, 256 >> n, ll << (n * 9));
    return 0;
}
