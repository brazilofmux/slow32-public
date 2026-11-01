/* 7.12.7.5 sqrt: nonnegative square root, double and float */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.17g %.17g %.17g\n", sqrt(v(2.0)), sqrt(v(1e-310)), sqrt(v(1e300)));
    printf("%g %g %.9g\n", sqrt(v(0.0)), sqrt(v(144.0)), (double)sqrtf(2.0f));
    return 0;
}
