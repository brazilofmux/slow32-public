/* 7.6.2 feclearexcept, fetestexcept, feraiseexcept, fegetexceptflag, fesetexceptflag: on whatever flags are supported */
#include <fenv.h>
#include <stdio.h>

#pragma STDC FENV_ACCESS ON

int main(void) {
    fexcept_t f;
    printf("%d %d\n", feclearexcept(FE_ALL_EXCEPT) == 0, fetestexcept(FE_ALL_EXCEPT) == 0);
    printf("%d %d\n", feraiseexcept(0) == 0, feclearexcept(0) == 0);
    printf("%d %d\n", fegetexceptflag(&f, FE_ALL_EXCEPT) == 0, fesetexceptflag(&f, FE_ALL_EXCEPT) == 0);
    int ok = 1;
#ifdef FE_DIVBYZERO
    feclearexcept(FE_ALL_EXCEPT);
    if (feraiseexcept(FE_DIVBYZERO) != 0 || fetestexcept(FE_ALL_EXCEPT) != FE_DIVBYZERO) ok = 0;
    feclearexcept(FE_DIVBYZERO);
    if (fetestexcept(FE_DIVBYZERO) != 0) ok = 0;
#endif
    printf("divbyzero-if-defined %d\n", ok);
    return 0;
}
