/* 7.12.5.5-7.12.5.6 sinh, tanh: relative accuracy for small arguments, where both are close to x */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g\n", sinh(v(1e-5)), sinh(v(-1e-7)));
    printf("%.12g %.12g\n", tanh(v(1e-8)), tanh(v(-1e-5)));
    return 0;
}
