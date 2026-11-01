/* 7.12.4 sin, cos: results for large arguments, where argument reduction must be done carefully */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    printf("%.9g\n", sin(v(1e6)));
    printf("%.9g\n", cos(v(1e15)));
    printf("%.9g\n", sin(v(1e22)));
    return 0;
}
