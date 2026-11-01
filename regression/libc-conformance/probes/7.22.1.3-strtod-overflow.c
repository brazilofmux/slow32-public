/* 7.22.1.3 strtod: overflow returns HUGE_VAL with the sign of the value and sets errno to ERANGE */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    unsigned long long b;
    errno = 0;
    b = dbits(strtod(s, &e));
    printf("%-26s +huge=%d -huge=%d max=%d erange=%d used=%d\n", s, b == 0x7ff0000000000000ull, b == 0xfff0000000000000ull,
           b == 0x7fefffffffffffffull, errno == ERANGE, (int)(e - s));
}
int main(void) {
    t("1e309"); t("-1e309"); t("1e99999999999"); t("0x1p1024"); t("-0x1p5000");
    t("1.7976931348623157e308");    /* the largest double, no error */
    t("1.7976931348623158e308");    /* below the halfway point: still the largest */
    t("1.7976931348623159e308");    /* above it: overflow */
    t("0x1.fffffffffffff8p1023");   /* halfway, rounds up out of range */
    t("0x1.fffffffffffff7p1023");   /* below halfway: the largest double */
    return 0;
}
