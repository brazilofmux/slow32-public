/* 7.12.4 sin, cos, tan: relative accuracy where the result is tiny or huge (sin at pi, cos at pi/2, tan at pi/2) */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double pi = 3.141592653589793, half = 1.5707963267948966;
    printf("%.6g\n", sin(v(pi)));
    printf("%.6g\n", cos(v(half)));
    printf("%.6g\n", tan(v(half)));
    printf("%.6g\n", sin(v(2 * pi)));
    return 0;
}
