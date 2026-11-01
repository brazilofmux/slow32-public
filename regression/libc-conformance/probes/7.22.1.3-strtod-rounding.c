/* 7.22.1.3 strtod: decimal input of at most DECIMAL_DIG digits is correctly rounded (recommended practice) */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    double d = strtod(s, 0);
    printf("%-26s %016llx\n", s, dbits(d));
}
int main(void) {
    t("0.1"); t("0.3"); t("1e23"); t("8.589973e9"); t("9007199254740993"); t("9007199254740995");
    t("2.2250738585072011e-308"); t("2.2250738585072014e-308"); t("4.9406564584124654e-324");
    t("2.4703282292062328e-324"); t("2.4703282292062327e-324");
    t("7.038531e-26"); t("1.448997445238699"); t("3.0540412e5"); t("5e-324"); t("1.7976931348623157e308");
    t("123456789012345678"); t("0.000001"); t("1e-7");
    return 0;
}
