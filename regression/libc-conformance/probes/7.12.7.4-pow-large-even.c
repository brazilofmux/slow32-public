/* 7.12.7.4 pow: a negative base to an integral exponent too large for int (even: positive, odd: negative) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%g\n", pow(v(-1.0), v(4294967296.0)));
    printf("%g\n", pow(v(-1.0), v(4294967297.0)));
    printf("%g\n", pow(v(-1.0), v(1e300)));
    return 0;
}
