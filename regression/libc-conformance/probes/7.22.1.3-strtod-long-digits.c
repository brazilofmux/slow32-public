/* 7.22.1.3 strtod: many-digit decimal input near a rounding boundary, and very long digit strings */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
static unsigned long long dbits(double d) { unsigned long long u; memcpy(&u, &d, sizeof u); return u; }

static char big[2100];
static void t(const char *s) {
    double d = strtod(s, 0);
    printf("%.30s %016llx\n", s, dbits(d));
}
int main(void) {
    int i;
    t("1.00000000000000011102230246251565404236316680908203125");   /* exactly halfway */
    t("1.00000000000000011102230246251565404236316680908203125000001");
    t("1.00000000000000011102230246251565404236316680908203124999999");
    t("9007199254740993.0000000000000000000000000000000000001");
    big[0] = '0'; big[1] = '.';
    for (i = 2; i < 2000; i++) big[i] = '0';
    big[2000] = '1';                                   /* 1e-1999: underflows */
    t(big);
    big[0] = '1';
    for (i = 1; i < 400; i++) big[i] = '0';
    big[400] = 0;                                      /* 1e399: overflows */
    t(big);
    for (i = 0; i < 1000; i++) big[i] = (i == 0) ? '1' : '0';
    big[1000] = 'e'; big[1001] = '-'; big[1002] = '9'; big[1003] = '9'; big[1004] = '9'; big[1005] = 0;
    t(big);                                            /* 1e0 exactly */
    return 0;
}
