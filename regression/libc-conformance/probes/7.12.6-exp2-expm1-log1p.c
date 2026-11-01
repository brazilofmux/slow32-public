/* 7.12.6.2, 7.12.6.3, 7.12.6.9 exp2, expm1, log1p: base-2 exponential and the near-zero forms */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g %.12g\n", exp2(v(10.0)), expm1(v(1e-10)), log1p(v(1e-10)));
    printf("%.5g %.5g %.5g\n", exp2f(3.0f), expm1f(1.0f), log1pf(1.0f));
    return 0;
}
