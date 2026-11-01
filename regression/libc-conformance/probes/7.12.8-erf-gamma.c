/* 7.12.8 erf, erfc, lgamma, tgamma: the error and gamma functions */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g\n", erf(v(0.5)), erfc(v(0.5)));
    printf("%.12g %.12g\n", lgamma(v(10.0)), tgamma(v(5.0)));
    return 0;
}
