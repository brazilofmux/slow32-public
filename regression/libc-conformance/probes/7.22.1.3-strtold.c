/* 7.22.1.3 strtold: the long double form converts like strtod for values double represents exactly */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    long double x = strtold(s, &e);
    printf("%-10s %016llx used=%d\n", s, dbits((double)x), (int)(e - s));
}
int main(void) {
    t("1.5"); t("-0x1p-3"); t("  2e10z"); t("abc"); t("0.25");
    return 0;
}
