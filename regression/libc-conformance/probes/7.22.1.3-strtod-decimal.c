/* 7.22.1.3 strtod: decimal subject sequences, exponents, signs, and where endptr is left */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    double d;
    errno = 0;
    d = strtod(s, &e);
    printf("%-12s %016llx used=%d errno0=%d\n", s, dbits(d), (int)(e - s), errno == 0);
}
int main(void) {
    t("1"); t("  -2.5"); t("+.5"); t("5."); t("1e3"); t("1E-2"); t("1e"); t("1e+");
    t("1e+x"); t("2.5e+2z"); t("-0"); t("0.1"); t("123.456e-2"); t("1.5.3");
    t("\v\f\r 7");
    return 0;
}
