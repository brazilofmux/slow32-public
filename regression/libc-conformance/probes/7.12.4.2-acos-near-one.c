/* 7.12.4.2 acos: relative accuracy just below 1, where the result is small */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g\n", acos(v(1 - 1e-12)));
    printf("%.12g\n", acos(v(0.9999999999)));
    return 0;
}
