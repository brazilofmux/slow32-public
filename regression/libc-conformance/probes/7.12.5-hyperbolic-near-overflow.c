/* 7.12.5.4-7.12.5.5 cosh, sinh: finite results just beyond exp's own overflow threshold (|x| = 710) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.6g %.6g %.6g\n", cosh(v(710.0)), sinh(v(710.0)), sinh(v(-710.0)));
    return 0;
}
