/* 7.12.5 cosh, sinh, tanh and float forms: values at ordinary arguments */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g %.12g\n", cosh(v(0.5)), cosh(v(-3.0)), cosh(v(0.0)));
    printf("%.12g %.12g %.12g\n", sinh(v(0.5)), sinh(v(-3.0)), sinh(v(20.0)));
    printf("%.12g %.12g %.12g\n", tanh(v(0.5)), tanh(v(-3.0)), tanh(v(30.0)));
    printf("%.5g %.5g %.5g\n", coshf((float)v(1.0)), sinhf((float)v(1.0)), tanhf((float)v(1.0)));
    return 0;
}
