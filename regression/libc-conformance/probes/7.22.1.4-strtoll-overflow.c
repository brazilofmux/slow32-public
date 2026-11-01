/* 7.22.1.4 strtoll, strtoull: 64-bit limits, ERANGE on overflow, unsigned wrap of a negated value */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

int main(void) {
    char *e;
    long long v;
    unsigned long long u;
    errno = 0; v = strtoll("9223372036854775807", &e, 10); printf("%lld %d\n", v, errno == ERANGE);
    errno = 0; v = strtoll("9223372036854775808", &e, 10); printf("%d %d %d\n", v == LLONG_MAX, errno == ERANGE, *e == 0);
    errno = 0; v = strtoll("-9223372036854775808", &e, 10); printf("%d %d\n", v == LLONG_MIN, errno == ERANGE);
    errno = 0; v = strtoll("-9223372036854775809", &e, 10); printf("%d %d\n", v == LLONG_MIN, errno == ERANGE);
    errno = 0; u = strtoull("18446744073709551615", &e, 10); printf("%llu %d\n", u, errno == ERANGE);
    errno = 0; u = strtoull("18446744073709551616", &e, 10); printf("%d %d %d\n", u == ULLONG_MAX, errno == ERANGE, *e == 0);
    errno = 0; u = strtoull("-1", &e, 10); printf("%d %d\n", u == ULLONG_MAX, errno == ERANGE);
    errno = 0; u = strtoull("-18446744073709551615", &e, 10); printf("%llu %d\n", u, errno == ERANGE);
    errno = 0; u = strtoull("-18446744073709551616", &e, 10); printf("%d %d\n", u == ULLONG_MAX, errno == ERANGE);
    errno = 0; v = strtoll("0x7FFFFFFFFFFFFFFF", &e, 0); printf("%d %d\n", v == LLONG_MAX, errno == 0);
    errno = 0; v = strtoll("-0x8000000000000000", &e, 16); printf("%d %d\n", v == LLONG_MIN, errno == 0);
    errno = 0; u = strtoull("1777777777777777777777", &e, 8); printf("%d %d\n", u == ULLONG_MAX, errno == 0);
    errno = 0; u = strtoull("2000000000000000000000", &e, 8); printf("%d %d\n", u == ULLONG_MAX, errno == ERANGE);
    return 0;
}
