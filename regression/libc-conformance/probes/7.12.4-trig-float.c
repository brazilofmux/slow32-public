/* 7.12.4 sinf, cosf, tanf, asinf, acosf, atanf, atan2f: float forms at ordinary arguments */
#include <math.h>
#include <stdio.h>

static float v(float x) { volatile float t = x; return t; }

int main(void) {
    printf("%.5g %.5g %.5g\n", sinf(v(0.5f)), cosf(v(0.5f)), tanf(v(0.5f)));
    printf("%.5g %.5g %.5g %.5g\n", asinf(v(0.5f)), acosf(v(0.5f)), atanf(v(2.0f)),
           atan2f(v(-1.0f), v(-2.0f)));
    return 0;
}
