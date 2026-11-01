/* 7.22.1.3 strtod: an underflowing result is no larger in magnitude than the smallest normalized value */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    unsigned long long b = dbits(strtod(s, &e));
    printf("%-11s small=%d sign-ok=%d used=%d\n", s, (b & 0x7fffffffffffffffull) <= 0x0010000000000000ull,
           s[0] == '-' || (b >> 63) == 0, (int)(e - s));
}
int main(void) {
    t("1e-400"); t("-1e-400"); t("1e-99999999"); t("0x1p-1080"); t("3e-310");
    return 0;
}
