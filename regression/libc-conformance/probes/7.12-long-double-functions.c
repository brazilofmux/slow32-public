/* 7.12 the long double forms (sinl, expl, sqrtl, fabsl, floorl, fmodl, ...) beside each double function */
#include <math.h>
#include <stdio.h>

int main(void) {
    volatile long double x = 2.0L;
    printf("%.6g %.6g %.6g\n", (double)sqrtl(x), (double)fabsl(-x), (double)floorl(x + 0.5L));
    printf("%.6g %.6g %.6g\n", (double)sinl(x), (double)expl(x), (double)fmodl(x, 1.5L));
    return 0;
}
