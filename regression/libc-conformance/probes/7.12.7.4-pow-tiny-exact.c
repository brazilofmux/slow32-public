/* 7.12.7.4 pow: exact powers of two below DBL_MIN are representable and must not come back zero */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.17g\n", pow(v(2.0), v(-1030.0)));
    printf("%.17g\n", pow(v(2.0), v(-1074.0)));
    printf("%.17g\n", pow(v(0.5), v(1060.0)));
    return 0;
}
