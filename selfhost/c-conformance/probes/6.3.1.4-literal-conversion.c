// 6.3.1.4p1  a floating literal converted to an integer type: a fraction dropped or a value out of range, clang warns
// expect: warnings
#include <stdio.h>
static int whole = 3.0;
static int frac = 2.5;
static _Bool bt = 0.25;
static _Bool bone = 1.0;
static int folded = 1.0 / 4;
static int never(void) {
    short s = 1e5;
    unsigned u = -1.0;
    return s + (int)u;
}
int main(void) {
    int n = -0.75;
    long long w = 3e9;
    printf("%d %d %d %d %d %d %lld\n", whole, frac, bt, bone, folded, n, w);
    if (n > 5) return never();
    return 0;
}
