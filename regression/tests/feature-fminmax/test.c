/* fmax, fmin and fdim (C99 7.12.12), which the C library lacked until
 * 2026-10-20: a NaN argument is ignored, two NaNs give a NaN, infinities
 * order, and of two zeros fmax gives +0 and fmin -0 -- the standard leaves
 * that last open; this is macOS's answer and IEEE 754-2019's maximumNumber
 * (glibc returns the first argument).  Clang makes these calls the
 * llvm.maxnum/minnum intrinsics, which the backend could not lower. */
#include <math.h>
#include <stdio.h>
int main(void) {
    volatile double pz = 0.0, nz = -0.0, n = NAN, one = 1.0;
    volatile float fpz = 0.0f, fnz = -0.0f, fn = NAN;
    printf("fmax(-0,+0) %d fmax(+0,-0) %d fmin(-0,+0) %d fmin(+0,-0) %d\n", signbit(fmax(nz, pz)) != 0, signbit(fmax(pz, nz)) != 0, signbit(fmin(nz, pz)) != 0, signbit(fmin(pz, nz)) != 0);
    printf("fmaxf(-0,+0) %d fminf(+0,-0) %d\n", signbit(fmaxf(fnz, fpz)) != 0, signbit(fminf(fpz, fnz)) != 0);
    printf("fmax(nan,1) %g fmin(1,nan) %g fmax(nan,nan) nan %d fmaxf(nan,2) %g\n", fmax(n, one), fmin(one, n), isnan(fmax(n, n)), fmaxf(fn, 2.0f));
    printf("fdim(5,3) %g fdim(3,5) %g fdim(nan,1) nan %d fdimf(2,1) %g\n", fdim(5.0, 3.0), fdim(3.0, 5.0), isnan(fdim(n, one)), fdimf(2.0f, 1.0f));
    printf("fmax(-inf,-1e308) %g fmin(inf,3) %g\n", fmax(-INFINITY, -1e308), fmin(INFINITY, 3.0));
    return 0;
}
