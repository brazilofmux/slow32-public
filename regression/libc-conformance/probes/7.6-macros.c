/* 7.6p6-p9 FE_ALL_EXCEPT, FE_DFL_ENV, rounding macros: the supported ones are defined; FE_ALL_EXCEPT ORs the flags */
#include <fenv.h>
#include <stdio.h>

int main(void) {
    int all = 0, rounding = 0;
#ifdef FE_DIVBYZERO
    all |= FE_DIVBYZERO;
#endif
#ifdef FE_INEXACT
    all |= FE_INEXACT;
#endif
#ifdef FE_INVALID
    all |= FE_INVALID;
#endif
#ifdef FE_OVERFLOW
    all |= FE_OVERFLOW;
#endif
#ifdef FE_UNDERFLOW
    all |= FE_UNDERFLOW;
#endif
#ifdef FE_TONEAREST
    rounding = 1;
#endif
    printf("%d %d %d\n", (FE_ALL_EXCEPT & all) == all, rounding, FE_DFL_ENV == FE_DFL_ENV);
    return 0;
}
