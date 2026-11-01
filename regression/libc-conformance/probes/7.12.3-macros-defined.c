/* 7.12.3 classification: fpclassify, isfinite, isinf, isnan, isnormal and signbit are macros */
#include <math.h>
#include <stdio.h>

int main(void) {
    int n = 0;
#ifdef fpclassify
    n |= 1;
#endif
#ifdef isfinite
    n |= 2;
#endif
#ifdef isinf
    n |= 4;
#endif
#ifdef isnan
    n |= 8;
#endif
#ifdef isnormal
    n |= 16;
#endif
#ifdef signbit
    n |= 32;
#endif
    printf("%d\n", n);
    return 0;
}
