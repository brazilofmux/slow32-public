/* 7.22.1.4 strtol: out-of-range input returns LONG_MAX or LONG_MIN, sets ERANGE, and consumes every digit */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>

static void t(const char *s) {
    char *e;
    long v;
    errno = 0;
    v = strtol(s, &e, 10);
    printf("max=%d min=%d erange=%d all=%d\n", v == LONG_MAX, v == LONG_MIN, errno == ERANGE, *e == 0);
}
int main(void) {
    char b[64];
    sprintf(b, "%ld", LONG_MAX); t(b);
    b[strlen(b) - 1]++; t(b);                 /* LONG_MAX + 1 (LONG_MAX ends in 7) */
    sprintf(b, "%ld", LONG_MIN); t(b);
    b[strlen(b) - 1]++; t(b);                 /* LONG_MIN - 1 (ends in 8) */
    t("99999999999999999999999999999999");
    t("-99999999999999999999999999999999");
    errno = 0;
    printf("hex: %d %d\n", strtol("0x7fffffffffffffffffff", 0, 0) == LONG_MAX, errno == ERANGE);
    return 0;
}
