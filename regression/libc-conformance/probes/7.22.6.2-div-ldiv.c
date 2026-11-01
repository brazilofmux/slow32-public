/* 7.22.6.2 div, ldiv: quotient truncated toward zero, remainder with the dividend sign */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

static void d(int n, int m) { div_t r = div(n, m); printf("div(%d,%d) = %d r %d\n", n, m, r.quot, r.rem); }
static void ld(long n, long m) { ldiv_t r = ldiv(n, m); printf("ldiv(%ld,%ld) = %ld r %ld\n", n, m, r.quot, r.rem); }
int main(void) {
    d(7, 2); d(-7, 2); d(7, -2); d(-7, -2); d(0, 5); d(INT_MIN, 1); d(INT_MAX, -1); d(INT_MIN + 1, 7);
    ld(7, 2); ld(-7, 2); ld(7, -2); ld(-7, -2); ld(-2000000000L, 3);
    {
        ldiv_t r = ldiv(LONG_MIN, 2);
        printf("%d %d\n", r.quot == LONG_MIN / 2, r.rem == 0);
    }
    return 0;
}
