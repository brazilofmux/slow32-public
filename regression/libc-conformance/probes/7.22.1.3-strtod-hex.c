/* 7.22.1.3 strtod: hexadecimal floating subject sequences, optional binary exponent, endptr placement */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    double d = strtod(s, &e);
    printf("%-14s %016llx used=%d\n", s, dbits(d), (int)(e - s));
}
int main(void) {
    t("0x1p4"); t("0X1.8P1"); t("0x.8p0"); t("0xA"); t("-0x10"); t("0x1p-2");
    t("0xff.8"); t("0x1p"); t("0x1p+"); t("0x"); t("0xg"); t(" 0x1P-1074");
    return 0;
}
