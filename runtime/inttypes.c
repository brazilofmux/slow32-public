/* inttypes.c -- C11 7.8.2, for both C libraries */
#include <inttypes.h>
#include <stdlib.h>

intmax_t imaxabs(intmax_t j) {
    return j < 0 ? -j : j;
}

/* the quotient truncates toward zero, as / does (7.8.2.2, 6.5.5p6) */
imaxdiv_t imaxdiv(intmax_t numer, intmax_t denom) {
    imaxdiv_t r;
    r.quot = numer / denom;
    r.rem = numer % denom;
    return r;
}

/* intmax_t is long long */
intmax_t strtoimax(const char *nptr, char **endptr, int base) {
    return strtoll(nptr, endptr, base);
}

uintmax_t strtoumax(const char *nptr, char **endptr, int base) {
    return strtoull(nptr, endptr, base);
}
