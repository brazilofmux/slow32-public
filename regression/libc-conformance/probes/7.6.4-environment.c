/* 7.6.4 fegetenv, feholdexcept, fesetenv, feupdateenv: saving, holding and restoring the environment succeed */
#include <fenv.h>
#include <stdio.h>

#pragma STDC FENV_ACCESS ON

int main(void) {
    fenv_t e, h;
    printf("%d\n", fegetenv(&e) == 0);
    printf("%d\n", feholdexcept(&h) == 0);
    printf("%d\n", fesetenv(&e) == 0);
    printf("%d\n", feupdateenv(&h) == 0);
    printf("%d %d\n", fesetenv(FE_DFL_ENV) == 0, fegetround() == FE_TONEAREST);
    return 0;
}
