/* 7.22.1.4 strtol: bases 2 to 36, letters of either case as digits, a digit not below the base ends the number */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static void t(const char *s, int base) {
    char *e;
    long v = strtol(s, &e, base);
    printf("%-8s base %2d -> %ld used=%d\n", s, base, v, (int)(e - s));
}
int main(void) {
    t("1011", 2); t("1012", 2); t("777", 8); t("789", 8); t("zz", 36); t("ZZ", 36); t("Zz9", 36);
    t("ff", 16); t("0xff", 16); t("0Xff", 16); t("10", 3); t("aB", 12); t("ab", 11); t("z", 35);
    t("\t\n +12", 10); t("- 1", 10); t("--1", 10); t("+-1", 10);
    return 0;
}
