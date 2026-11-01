/* 7.12.6.4 frexp: fraction in [1/2, 1) times a power of two, both parts zero for zero, subnormals too */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

int main(void) {
    double in[] = { 8.0, -3.0, 0.1, 1.0, 1e300, 4.9406564584124654e-324, 1e-310 };
    for (int i = 0; i < 7; i++) {
        int e = 12345;
        double f = frexp(v(in[i]), &e);
        printf("%.17g %d %d\n", f, e, ldexp(f, e) == in[i]);
    }
    int e = 12345;
    double f = frexp(v(0.0), &e);
    printf("zero %d %d\n", f == 0, e);
    float ff = frexpf(-48.0f, &e);
    printf("%g %d\n", (double)ff, e);
    ff = frexpf((float)v(1e-40), &e);
    printf("%.9g %d\n", (double)ff, e);
    return 0;
}
