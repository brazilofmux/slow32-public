/* x**n by multiplication.  Volatile, or the optimizer folds what this is here to run:
 * without a powi libcall LLVM called pow with the exponent converted to the base's type,
 * and an odd exponent past 2^24 doesn't survive that in a float: (-1.0f)**16777217 was +1. */
#include <stdio.h>

static volatile float fx;
static volatile double dx;
static volatile int nx;

static unsigned fbits(float f) {
    union { float f; unsigned u; } v;
    v.f = f;
    return v.u;
}

static void fcase(float x, int n) {
    fx = x;
    nx = n;
    printf("f %d %08x\n", n, fbits(__builtin_powif(fx, nx)));
}

static void dcase(double x, int n) {
    union { double d; unsigned u[2]; } v;
    dx = x;
    nx = n;
    v.d = __builtin_powi(dx, nx);
    printf("d %d %08x%08x\n", n, v.u[1], v.u[0]);
}

int main(void) {
    fcase(-1.0f, 16777217);
    fcase(-1.0f, -16777217);
    fcase(-1.0f, 2147483647);
    fcase(-1.0f, 16777216);
    fcase(3.3f, 7);
    fcase(3.3f, -7);
    fcase(2.0f, 0);
    fcase(0.0f, 0);
    fcase(2.0f, 127);
    fcase(2.0f, 128);
    fcase(2.0f, -149);
    dcase(-1.0, 2147483647);
    dcase(-1.0, -2147483647);
    dcase(3.3, 7);
    dcase(3.3, -7);
    dcase(2.0, 1023);
    dcase(2.0, -1074);
    return 0;
}
