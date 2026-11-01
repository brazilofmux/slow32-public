/* 7.22.1.3 strtod: hexadecimal input with more bits than double holds is rounded to nearest, ties to even */
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
    t("0x1.00000000000008p0");    /* exactly half an ulp: to even, 1.0 */
    t("0x1.00000000000018p0");    /* half above an odd value: up */
    t("0x1.000000000000081p0");   /* just above half: up */
    t("0x1.00000000000007fp0");   /* just below half: down */
    t("0x20000000000001");        /* 2^53+1: to even */
    t("0x1.fffffffffffff7ffp1023");
    t("0x0.0000000000001p-1022"); /* smallest subnormal */
    return 0;
}
