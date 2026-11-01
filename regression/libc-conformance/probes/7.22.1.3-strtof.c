/* 7.22.1.3 strtof: correctly rounded float results, including where rounding through double goes wrong */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long fbits(float f) { unsigned int u; memcpy(&u, &f, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    float f = strtof(s, &e);
    printf("%-30s %08lx used=%d\n", s, fbits(f), (int)(e - s));
}
int main(void) {
    t("0.1"); t("1.5"); t("-0x1.8p1"); t("3.4028235e38");
    t("1.000000059604644775390625");        /* exactly halfway: to even */
    t("1.00000005960464477539062501");      /* above halfway: up (strtod then cast gets this wrong) */
    t("0x1.000001p0");                      /* hexadecimal, halfway: to even */
    t("0x1.0000010000000001p0");            /* just above halfway */
    t("1.17549435082228750797e-38"); t("1.4e-45"); t("inf"); t("-inf");
    return 0;
}
