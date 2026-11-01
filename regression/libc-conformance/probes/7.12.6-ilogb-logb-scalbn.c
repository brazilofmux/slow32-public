/* 7.12.6.5, 7.12.6.11, 7.12.6.13 ilogb, logb, scalbn, scalbln: exponent extraction and scaling by FLT_RADIX */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%d %g\n", ilogb(v(1000.0)), logb(v(1000.0)));
    printf("%g %g\n", scalbn(v(1.5), 4), scalbln(v(1.5), -2L));
    return 0;
}
