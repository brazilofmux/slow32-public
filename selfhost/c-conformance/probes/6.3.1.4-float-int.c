// 6.3.1.4p1-p2  float to integer truncates toward zero; integer to float converts
#include <stdio.h>
int main(void) {
    volatile double a = 3.99, b = -3.99, c = 4294967295.0, e = 1e10;
    volatile float f = -0.75f;
    int ia = (int)a, ib = (int)b, ifl = (int)f;
    unsigned uc = (unsigned)c;
    long long le = (long long)e;
    unsigned long long ule = (unsigned long long)1e19;
    volatile unsigned big = 4000000000u;
    double du = big;
    volatile long long nl = -9007199254740993LL;
    double dl = (double)nl;
    float ff = 16777217;            /* not exactly representable: nearest */
    printf("%d %d %d %u %lld %llu %.1f %.1f %.1f\n", ia, ib, ifl, uc, le, ule, du, dl, (double)ff);
    return 0;
}
