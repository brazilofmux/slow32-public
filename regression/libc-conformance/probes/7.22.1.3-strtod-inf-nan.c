/* 7.22.1.3 strtod: INF, INFINITY and NAN spellings, any case, with optional NAN(n-char-sequence) */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static void t(const char *s) {
    char *e;
    unsigned long long b = dbits(strtod(s, &e));
    int inf = (b << 1) == 0xffe0000000000000ull, nan = (b << 1) > 0xffe0000000000000ull;
    printf("%-12s inf=%d nan=%d neg=%d used=%d\n", s, inf, nan, (int)(b >> 63), (int)(e - s));
}
int main(void) {
    t("inf"); t("INF"); t("-Inf"); t("infinity"); t("InFiNiTy!"); t("infin"); t("infx");
    t("nan"); t("NAN"); t("nan(123)"); t("NaN(abc_9)x"); t("nan("); t("nan(1"); t("nan()"); t("-nan");
    t("  +inf"); t("in");
    return 0;
}
