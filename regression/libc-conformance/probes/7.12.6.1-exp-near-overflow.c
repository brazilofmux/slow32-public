/* 7.12.6.1 exp: finite results up to the largest double (exp(709.7) is about 1.65e308) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.6g %.6g %.6g\n", exp(v(709.0)), exp(v(709.5)), exp(v(709.78)));
    return 0;
}
