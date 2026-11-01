/* 7.12.7.1 cbrt: real cube root, negative arguments included */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g %.12g %.12g\n", cbrt(v(27.0)), cbrt(v(-8.0)), cbrt(v(2.0)), cbrt(v(-1e-300)));
    printf("%.12g %.5g\n", cbrt(v(1e300)), (double)cbrtf(-64.0f));
    return 0;
}
