/* 7.12.9.6 round: integral arguments at and above 2^52 come back unchanged */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double in[] = { 4503599627370497.0, -4503599627370497.0, 4503599627370495.5, 9007199254740993.0, 1e300 };
    for (int i = 0; i < 5; i++)
        printf("%.17g -> %.17g\n", in[i], round(v(in[i])));
    printf("%.9g\n", (double)roundf(8388609.0f));
    return 0;
}
