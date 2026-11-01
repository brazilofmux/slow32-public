/* 7.12.4 sin, cos, tan, asin, acos, atan, atan2: values at ordinary arguments, to 12 significant digits */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.12g %.12g %.12g\n", sin(v(0.5)), sin(v(-2.0)), sin(v(3.0)));
    printf("%.12g %.12g %.12g\n", cos(v(0.5)), cos(v(-2.0)), cos(v(10.0)));
    printf("%.12g %.12g %.12g\n", tan(v(0.5)), tan(v(-2.0)), tan(v(1.5)));
    printf("%.12g %.12g %.12g\n", asin(v(0.5)), asin(v(-0.9)), asin(v(1.0)));
    printf("%.12g %.12g %.12g\n", acos(v(0.5)), acos(v(-0.9)), acos(v(-1.0)));
    printf("%.12g %.12g %.12g\n", atan(v(0.5)), atan(v(-3.0)), atan(v(1e10)));
    printf("%.12g %.12g %.12g %.12g\n", atan2(v(1.0), v(1.0)), atan2(v(1.0), v(-1.0)),
           atan2(v(-1.0), v(-1.0)), atan2(v(-2.0), v(0.5)));
    return 0;
}
