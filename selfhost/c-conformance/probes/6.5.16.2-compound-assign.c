// 6.5.16.2p3  E1 op= E2 is E1 = E1 op E2 with E1 evaluated once; narrow and float operands
#include <stdio.h>
static int calls;
static int idx(void) { calls++; return 1; }
int main(void) {
    int a[3] = {10, 20, 30};
    a[idx()] += 5;
    a[idx()] <<= 2;
    unsigned char uc = 250; uc += 10;
    int i = 7; i *= 2.5;
    short s = 100; s /= 3; s %= 7;
    double d = 1; d -= 0.25;
    int *p = a; p += 2;
    long long ll = 1; ll <<= 35; ll |= 3; ll ^= 1;
    unsigned u = 0xF0; u &= 0x3C; u >>= 2;
    printf("%d %d %d %d %d %.2f %d %lld %u\n", a[1], calls, uc, i, s, d, *p, ll, u);
    return 0;
}
