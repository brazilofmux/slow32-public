/* 7.12.6.12 modf: integral and fractional parts, each with the argument's sign, negative zero included */
#include <math.h>
#include <stdio.h>

static double v(double x) { volatile double t = x; return t; }

static void show(double x) {
    double ip;
    double fp = modf(v(x), &ip);
    printf("%g: %g %g signs %d %d\n", x, ip, fp, copysign(1.0, ip) < 0, copysign(1.0, fp) < 0);
}

int main(void) {
    show(3.75);
    show(-3.75);
    show(-3.0);
    show(-0.5);
    show(1e20);
    float fi;
    float ff = modff(-2.25f, &fi);
    printf("%g %g\n", (double)fi, (double)ff);
    return 0;
}
