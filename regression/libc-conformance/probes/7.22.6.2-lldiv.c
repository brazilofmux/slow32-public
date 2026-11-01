/* 7.22.6.2 lldiv: the long long form, quotient truncated toward zero */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

static void t(long long n, long long m) { lldiv_t r = lldiv(n, m); printf("lldiv(%lld,%lld) = %lld r %lld\n", n, m, r.quot, r.rem); }
int main(void) {
    t(7, 2); t(-7, 2); t(7, -2); t(-7, -2); t(-10000000000007LL, 1000); t(LLONG_MIN, 3); t(LLONG_MAX, -10);
    return 0;
}
