/* 7.6.3 fegetround, fesetround: the default direction is reported, set again, and a bogus one refused */
#include <fenv.h>
#include <stdio.h>

#pragma STDC FENV_ACCESS ON

int main(void) {
    int r = fegetround();
    printf("%d\n", r == FE_TONEAREST);
    printf("%d %d\n", fesetround(FE_TONEAREST) == 0, fegetround() == FE_TONEAREST);
    printf("%d %d\n", fesetround(0x7f5a) != 0, fegetround() == FE_TONEAREST);
    return 0;
}
