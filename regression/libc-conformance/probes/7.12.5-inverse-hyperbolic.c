/* 7.12.5.1-7.12.5.3 acosh, asinh, atanh: the inverse hyperbolic functions */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g %.12g\n", acosh(v(2.0)), asinh(v(-1.5)), atanh(v(0.5)));
    printf("%.5g %.5g %.5g\n", acoshf(2.0f), asinhf(-1.5f), atanhf(0.5f));
    return 0;
}
