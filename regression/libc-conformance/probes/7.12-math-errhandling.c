/* 7.12p9, 7.12.1 math_errhandling: domain, pole and overflow errors are reported the way math_errhandling says */
#include <errno.h>
#include <fenv.h>
#include <math.h>
#include <stdio.h>

/* the FE_ exception macros exist only where the exceptions do (7.6p6) */
#ifndef FE_INVALID
#define FE_INVALID 0
#endif
#ifndef FE_DIVBYZERO
#define FE_DIVBYZERO 0
#endif
#ifndef FE_OVERFLOW
#define FE_OVERFLOW 0
#endif

static int reported(int e, int flag) {
    int r = 0;
    if ((math_errhandling & MATH_ERRNO) && errno == e) r = 1;
    if ((math_errhandling & MATH_ERREXCEPT) && fetestexcept(flag)) r = 1;
    return r;
}

int main(void) {
    volatile double m1 = -1.0, z = 0.0, big = 1000.0;
    volatile double r;
    printf("%d\n", (math_errhandling & (MATH_ERRNO | MATH_ERREXCEPT)) != 0);
    errno = 0; feclearexcept(FE_ALL_EXCEPT);
    r = sqrt(m1);
    printf("sqrt(-1) %d\n", reported(EDOM, FE_INVALID));
    errno = 0; feclearexcept(FE_ALL_EXCEPT);
    r = log(z);
    printf("log(0) %d\n", reported(ERANGE, FE_DIVBYZERO));
    errno = 0; feclearexcept(FE_ALL_EXCEPT);
    r = exp(big);
    printf("exp(1000) %d %d\n", reported(ERANGE, FE_OVERFLOW), r == HUGE_VAL);
    (void)r;
    return 0;
}
