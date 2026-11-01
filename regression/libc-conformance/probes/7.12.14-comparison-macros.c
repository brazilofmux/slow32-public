/* 7.12.14 isgreater, isgreaterequal, isless, islessequal, islessgreater, isunordered: quiet comparisons */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

int main(void) {
    volatile double a = 1.0, b = 2.0;
    double n = strtod("nan", 0);
    printf("%d %d %d %d %d %d\n", isgreater(a, b) != 0, isgreaterequal(b, a) != 0, isless(a, b) != 0,
           islessequal(a, a) != 0, islessgreater(a, b) != 0, isunordered(a, b) != 0);
    printf("%d %d %d %d %d %d\n", isgreater(a, n) != 0, isgreaterequal(n, a) != 0, isless(n, n) != 0,
           islessequal(a, n) != 0, islessgreater(n, b) != 0, isunordered(a, n) != 0);
    return 0;
}
