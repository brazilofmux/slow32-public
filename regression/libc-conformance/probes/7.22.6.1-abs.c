/* 7.22.6.1 abs, labs, llabs: absolute values of representable results */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

int main(void) {
    printf("%d %d %d %d\n", abs(-5), abs(5), abs(0), abs(-INT_MAX) == INT_MAX);
    printf("%ld %d\n", labs(-123456L), labs(-LONG_MAX) == LONG_MAX);
    printf("%lld %lld\n", llabs(-9000000000LL), llabs(-LLONG_MAX));
    return 0;
}
