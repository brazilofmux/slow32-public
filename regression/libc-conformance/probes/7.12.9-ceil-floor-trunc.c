/* 7.12.9.1, 7.12.9.2, 7.12.9.8 ceil, floor, trunc: exact integral results, near 2^52 and beyond included (sign of a zero result not tested) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double in[] = { 2.5, -2.5, -0.5, 0.25, 4503599627370495.5, -4503599627370495.5,
                    9007199254740993.0, 1e300, -7.0 };
    for (int i = 0; i < 9; i++)
        printf("%.17g: %.17g %.17g %.17g\n", in[i], ceil(v(in[i])) + 0.0,
               floor(v(in[i])) + 0.0, trunc(v(in[i])) + 0.0);
    printf("%.9g %.9g %.9g\n", (double)ceilf(8388607.5f), (double)floorf(-8388607.5f),
           (double)truncf(-1.75f));
    return 0;
}
