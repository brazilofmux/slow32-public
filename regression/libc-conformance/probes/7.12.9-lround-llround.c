/* 7.12.9.7 lround, llround: round halfway away from zero to an integer type */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%ld %ld %lld\n", lround(v(2.5)), lround(v(-2.5)), llround(v(1e15 + 0.5)));
    printf("%ld\n", lroundf(-1.5f));
    return 0;
}
