/* 7.12.9.3-7.12.9.5 nearbyint, rint, lrint, llrint: round in the current rounding direction (to nearest, ties even) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%g %g %g\n", nearbyint(v(2.5)), rint(v(3.5)), rint(v(-2.5)));
    printf("%ld %lld\n", lrint(v(2.5)), llrint(v(-3.5)));
    printf("%g %g %ld\n", (double)nearbyintf(0.5f), (double)rintf(1.5f), lrintf(4.5f));
    return 0;
}
