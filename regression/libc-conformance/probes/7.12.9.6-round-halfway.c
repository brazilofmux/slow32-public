/* 7.12.9.6 round: nearest integer, halfway cases away from zero, the value just below one half included (sign of a zero result not tested) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double in[] = { 0.5, -0.5, 1.5, 2.5, -2.5, 0.49999999999999994, -0.49999999999999994, 2.4999999999999996 };
    for (int i = 0; i < 8; i++)
        printf("%.17g -> %g\n", in[i], round(v(in[i])) + 0.0);
    printf("%g %g %g\n", (double)roundf(2.5f), (double)roundf(-0.5f), (double)roundf(0.49999997f));
    return 0;
}
